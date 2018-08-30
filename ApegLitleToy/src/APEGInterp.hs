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
           | VLan [ApegRule]
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
                    

isOk :: APegSt (Bool)
isOk = get >>= \(grm,env,tyEnv,reckon,inp,res) -> return res

done :: APegSt ()
done =  get >>= \(grm,env,tyEnv,reckon,inp,res) -> put (grm,env,tyEnv,reckon,inp,True) 

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


unMeta :: MAPeg -> APegSt (APeg)
unMeta MkLambda           = return $ Lambda
unMeta (MkCal nt inh syn) = do  xs <- mapM evalExp inh 
                                ys <- mapM evalExp syn
                                return $ NT nt (map expFromVal xs) (map expFromVal ys)
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
evalExp (ExtRule lam ntexp mapeg) =  do grm <- evalExp lam
                                        nt <- evalExp ntexp
                                        apeg <- evalExp mapeg -- Dyn typing to be done here !
                                        dynRule grm nt apeg 
                                        
evalExp (MkRule nt inh syn b) = do xs <- mapM evalExp syn
                                   apeg <- evalExp b
                                   ruleCreate nt inh xs apeg 
evalExp (Str s) = return (VStr s)
evalExp (EmptyMap) = return (VMap M.empty)
evalExp (EVar v)   = var v
evalExp (MetaPeg m) = unMeta m >>=  return.VPeg
evalExp (MetaExp m) = return $ VExp m
evalExp (MpLit xs)  = (mapM (\(s,b) -> (evalExp b>>= (\r->return (s,r)))) xs) >>= (\xs -> return $ VMap (M.fromList xs))
evalExp (MapIns m s v)  = do mp <- evalExp m
                             val <- evalExp v
                             mapInsert mp s val
evalExp (MapAcces m i) = do mp <- evalExp m
                            str <- evalExp i
                            mapAcces mp str

                                
                                
-- ======= APEG Interpretation of PEG Expression ======= --

interp :: APeg -> APegSt ()
interp (Lambda) =  done
interp (Lit s)  = patternMatch s




