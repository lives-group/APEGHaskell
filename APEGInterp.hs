module APEGInterp where

import AbstractSyntax
import qualified Data.Map as M
import Data.Either
import Control.Monad.State.Lazy


type VEnv  = M.Map String Value 
type TyEnv = M.Map String ([Type],[Type], TyRuleEnv) 
type TyRuleEnv = M.Map String Type -- All the variables in the scope of a rule
type Error   = [Int]

type Input = String

type ApegTuple = (ApegGrm, -- The "Current" Grammar (Initially g0)
                  VEnv,  -- A stack of value enviroments   
                  TyEnv,   -- A Type Enviroment of rule and its variables. 
                  String,  -- the current string accept by the parser on the current rule.
                  Input,   -- The current input state. 
                  Bool)    -- The Result of las operation

-- State for the APEG interpreter: 
type APegSt = State (ApegTuple) 

data Value = VStr String
           | VMap VEnv
           | VLan ApegGrm
           | VPeg APeg
           | VExp Expr
           | Undefined
           deriving Show

zeroSt :: ApegGrm -> String -> ApegTuple
zeroSt grm s = (grm,M.empty ,M.empty, [],s,True)

valIsMExpr :: Value -> Bool
valIsMExpr (VExp _) = True
valIsMExpr _        = False

valIsMPeg :: Value -> Bool
valIsMPeg (VPeg _) = True
valIsMPeg _        = False

expFromVal :: Value -> Expr
expFromVal (VExp e) = e

varNameFromVal :: Value -> Var
varNameFromVal (VExp (Str  s)) = s


apegFromVal :: Value -> APeg
apegFromVal (VPeg e) = e


matchAll :: [Type] -> [Type] -> Bool
matchAll xs ys = (and $ zipWith (==) xs ys) && ((length xs) == (length ys))


-- inferType :: TyRuleEnv -> MExpr -> Type
-- inferType re (MkStr _)    = TyStr
-- inferType re (MkEmptyMap) = TyMap TyStr
-- inferType re (MkVar v)    = maybe (re !? v) ()

-- unMetaE :: MExpr -> Expr
-- unMetaE (MkStr s)  = Str s
-- unMetaE (MkEmptyMap) = EmptyMap
-- unMetaE (MkVar v) = EVar v
-- unMetaE (MkMp xs) = Mp (map (\(v,e) -> (v,unMetaE e)) xs)
-- unMetaE (MkMapIns eb s ee) = MapIns (unMetaE eb) s (unMetaE ee)
-- unMetaE (MkMapAcces me ma ) = MapAcces (unMetaE me) (unMetaE ma) 


-- =================== State TAD related Functions =================== --

-- Projections:

language :: APegSt (ApegGrm)
language = get >>= \(grm,env,tyEnv,reckon,inp,res) -> return grm

var :: String -> APegSt Value
var s = get >>= (\(grm,env,tyEnv,reckon,inp,res) -> case env M.!? s of 
                                                      Nothing -> fail ("Undefined varibale " ++ s)
                                                      Just v   -> return v)

envAlter :: (VEnv -> VEnv) -> APegSt ()
envAlter t = get >>= (\(grm,env,tyEnv,reckon,inp,res) -> put (grm,t env,tyEnv,reckon,inp,res)) 
                                                      
-- updateVar :: (String,Value) ->  APegSt ()
-- updateVar (k,v) = 
                                                      
envSwap :: VEnv ->  APegSt (VEnv) 
envSwap newEnv = do (g,env,tyEnv,reckon,inp,res) <- get
                    put(g,newEnv,tyEnv,reckon,inp,res) 
                    return env
                     
isOk :: APegSt (Bool)
isOk = get >>= \(grm,env,tyEnv,reckon,inp,res) -> return res




onSucess :: APegSt a -> APegSt a -> APegSt a
onSucess suc fai = isOk >>= f
    where f True  = suc
          f False = fai
                  

done :: APegSt ()
done =  modify (\(grm,env,tyEnv,reckon,inp,res) -> (grm,env,tyEnv,reckon,inp,True)) 

pfail :: APegSt ()
pfail = modify (\(grm,env,tyenv,reckon,inp,r) -> (grm,env,tyenv,reckon,inp,False))

ruleCreate :: String -> [(Type,Var)] -> [Value] -> Value -> APegSt (Value)
ruleCreate s inh syn b
  | (all valIsMExpr syn) && (valIsMPeg b) = return $ VLan [ApegRule s inh (map expFromVal syn) (apegFromVal b)]
  | otherwise = fail "Unproper attempt to create a new rule !"

dynRule :: Value -> Value -> Value -> APegSt Value
dynRule (VLan grm) (VStr nt) (VPeg p) = return $ VLan (grmExtRule grm nt p)
dynRule _ _ _ = fail "Unproper attempt to compose an existing rule !"


-- =================== Monad Utilities =================== --

lanUnion :: Value -> Value -> APegSt Value
lanUnion (VLan l1) (VLan l2) = return $ VLan (l1++l2)
lanUnion _ _ = fail "Attempt to unite two values that aren't languages"

unMeta :: MAPeg -> APegSt (APeg)
unMeta MkLambda           = return $ Lambda
unMeta (MkCal nt inh syn) = do xs <- mapM evalExp inh 
                               ys <- mapM evalExp syn
                               return $ NT nt (map expFromVal xs) (map varNameFromVal ys)
unMeta (MkKle e)          = evalExp e >>= return.(Kle).apegFromVal
unMeta (MkNot e)          = evalExp e >>= return.(Not).apegFromVal
unMeta (MkSeq x y)        = evalExp x >>= \px -> evalExp y >>= \py -> return $ Seq (apegFromVal px) ((apegFromVal py))
unMeta (MkAlt x y)        = evalExp x >>= \px -> evalExp y >>= \py -> return $ Alt ((apegFromVal px)) (apegFromVal py)
unMeta (MkAE xs)          = do ys <- mapM (\(v,e) -> evalExp e >>= (\r -> return (v,expFromVal r))) xs
                               return $ AEAttr ys

mapInsert :: Value -> String -> Value -> APegSt (Value)
mapInsert (VMap m) s v = return $ VMap (M.insert s v m)
mapinsert v _ _        = fail (" value: " ++ (show v) ++ " is not a map.") 

mapAcces :: Value -> Value -> APegSt (Value)
mapAcces (VMap m) (VStr s) = case (m M.!? s) of
                                 Just r -> return r
                                 Nothing -> return Undefined
mapAccess m      (VStr _)  = fail (" value: " ++ (show m) ++ " is not a map.") 
mapAccess m  x  = fail (" value: " ++ (show x) ++ " is not a string.") 

evalExp :: Expr -> APegSt (Value)
evalExp (EmptyMap) = return (VMap M.empty)
evalExp (Str s) = return (VStr s)
evalExp (EVar v)   = var v
evalExp (ExtRule lam ntexp mapeg) =  do grm <- evalExp lam
                                        nt <- evalExp ntexp
                                        apeg <- evalExp mapeg -- Dyn typing to be done here !
                                        dynRule grm nt apeg 
                                        
evalExp (MkRule nt inh syn b) = do xs <- mapM evalExp syn
                                   apeg <- evalExp b
                                   ruleCreate nt inh xs apeg 
evalExp (Union e1 e2) = do l1 <- evalExp e1
                           l2 <- evalExp e2
                           lanUnion l1 l2                         
evalExp (MetaPeg m) = unMeta m >>=  return.VPeg
evalExp (MetaExp m) = return $ VExp m
evalExp (MpLit xs)  = (mapM (\(s,b) -> (evalExp b>>= (\r->return (s,r)))) xs) >>= (\xs -> return $ VMap (M.fromList xs))
evalExp (MapIns m s v)  = do mp  <- evalExp m
                             val <- evalExp v
                             mapInsert mp s val
evalExp (MapAcces m i) = do mp  <- evalExp m
                            str <- evalExp i
                            mapAcces mp str

-- ======= APEG Interpretation of PEG Expression ( PAEG PARSER COMBINATORS)======= --

prefix :: String -> String -> (String,String)
prefix xs ys = test (splitAt (length xs) ys)
   where
       test (p,zs)
           | p == xs = (p,zs)
           | otherwise = ([],ys)

patternMatch :: String -> APegSt ()
patternMatch s = do (grm,env,tyEnv,reckon,inp,res) <- get
                    case prefix s inp of
                         ([],_) ->  put (grm,env,tyEnv,reckon,inp,False)
                         (xs,ys) -> put (grm,env,tyEnv,reckon++xs,ys,True)

implicitLanArg :: [Value] -> APegSt ([Value])
implicitLanArg [] = language >>= return.(:[]).VLan
implicitLanArg xs@((VLan _):_) = return xs
implicitLanArg (xs) = language >>= return.(:xs).VLan

try :: APegSt () -> APegSt ()
try p = do s' <- get
           r <- p
           onSucess (return ()) (put s' >> pfail)

klenne :: APegSt a -> APegSt [a]
klenne p =  
     do s <- get 
        x <- p 
        xs <- onSucess (klenne p) (put s >> done >> return [])
        return (x:xs)            

sequential :: APegSt () -> APegSt () -> APegSt ()
sequential p q = do p 
                    r <- isOk
                    if r then q else return () 
                    
alternate :: APegSt () -> APegSt () -> APegSt ()
alternate l r = try l >> onSucess (return ()) (done >> r) 
                   
notPeg :: APegSt () -> APegSt ()
notPeg p = do (_,_,_,reckon,inp,_) <- get 
              p
              (grm,env,tyEnv,_,_,res) <- get 
              onSucess (put (grm,env,tyEnv,reckon,inp,False)) (put (grm,env,tyEnv,reckon,inp,True) >> return ()) 
              
bind :: (Var, Expr) -> APegSt ()
bind (v,e) = evalExp e >>= \val -> envAlter (M.insert v val)
                   
supresEnv :: VEnv -> APegSt a -> APegSt a
supresEnv nwEnv b = do oldEnv <- envSwap nwEnv
                       r <- b
                       envSwap oldEnv
                       return r

envFromDec :: [(Type,Var)] -> [Value] -> VEnv
envFromDec xs vs = M.fromList $ zipWith (\(_,v) o -> (v,o)) xs vs

                                    
callNt :: String -> [Value] -> [Var] -> APegSt ()
callNt nt vs@((VLan g):inh) vars = 
   case fetch g nt of
     Just r  -> interpRule vs vars r 
     Nothing -> fail "Attempt to call inexisiting rule !"
callNt _ _ _ = fail "Panic !, Missing Language Attribute !" 

interp :: APeg -> APegSt ()
interp (Lambda)       =  done
interp (Lit s)        = patternMatch s
interp (NT s inh ret) = do inh <- (mapM evalExp inh >>= implicitLanArg)
                           callNt s inh ret 
interp (Kle p)        = klenne (interp p) >> return ()
interp (Seq e d)      = sequential (interp e) (interp d)
interp (Alt e d)      = alternate (interp e) (interp d)
interp (Not e)        = notPeg (interp e)
interp (AEAttr xs)    = mapM_ bind xs  


interpRule :: [Value] -> [Var] -> ApegRule -> APegSt ()
interpRule vs os (ApegRule nt inh syn b) = do zs <- supresEnv (envFromDec inh vs) 
                                                               (interp b >> 
                                                               (mapM evalExp syn >>= 
                                                               \ws -> return $ zip os ws))
                                              envAlter (M.union (M.fromList zs))

interpGrammar :: [Value] -> ApegGrm -> APegSt ()
interpGrammar vs [] = return ()
interpGrammar vs (r:_) = (implicitLanArg vs) >>= \vs' -> interpRule vs' (outs r) r  
    where outs (ApegRule _ _ syn _) = [ "_varOut" ++ (show i) | i <- [1..length syn] ]


