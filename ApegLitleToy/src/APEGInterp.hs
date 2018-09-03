module APEGInterp where
       
import AbstractSyntax
import APEGState
import qualified Data.Map as M
import APEGCombinators
-- import Data.Either
import Control.Monad.State.Lazy


bind :: (Var, Expr) -> APegSt ()
bind (v,e) = evalExp e >>= (varSet v) 

                                    
bindPeg :: Var -> APeg -> APegSt ()
bindPeg v p  = do prfx <- getStr
                  resetStr
                  interp p
                  res <- getStr
                  modify (\(g,e,t,r,i,k) -> (g,e,t,prfx++r,i,k))
                  varSet v (VStr res)
                                    
callNt :: String -> [Value] -> [Var] -> APegSt ()
callNt nt vs@((VLan g):inh) vars = 
   case fetch g nt of
     Just r  -> interpRule vs vars r 
     Nothing -> fail "Attempt to call inexisiting rule !"
callNt _ _ _ = fail "Panic !, Missing Language Attribute !" 

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
--evalExp (EmptyMap) = return (VMap M.empty)
evalExp (Str s) = return (VStr s)
evalExp (EVar v)   = var v
evalExp (ExtRule lam ntexp mapeg) =  do grm <- evalExp lam
                                        nt <- evalExp ntexp
                                        apeg <- evalExp mapeg -- Dyn typing to be done here !
                                        dynRule grm nt apeg 
                                        
evalExp (MkRule nt inh syn b) = do ntName <- evalExp nt
                                   xs <- mapM evalExp syn
                                   apeg <- evalExp b
                                   ruleCreate (strVal ntName) inh xs apeg 
evalExp (Union e1 e2) = do l1 <- evalExp e1
                           l2 <- evalExp e2
                           lanUnion l1 l2                         
evalExp (MetaPeg m) = unMeta m >>=  return.VPeg
evalExp (MetaExp m) = return $ VExp m
evalExp (MpLit xs)  = (mapM (\(s,b) -> (evalExp b>>= (\r->return (s,r)))) xs) >>= (\xs -> return $ VMap (M.fromList xs))
evalExp (MapIns m s v)  = do mp  <- evalExp m
                             key <- evalExp s
                             val <- evalExp v
                             mapInsert mp (varNameFromVal key) val
evalExp (MapAccess m i) = do mp  <- evalExp m
                             str <- evalExp i
                             mapAcces mp str

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
interp (Bind v p)     = bindPeg v p


interpRule :: [Value] -> [Var] -> ApegRule -> APegSt ()
interpRule vs os (ApegRule nt inh syn b) = do zs <- supresEnv (envFromDec inh vs) 
                                                              (interp b >> 
                                                               (mapM evalExp syn >>= 
                                                                \ws -> return $ zip os ws))
                                              varsSet zs
                                             
interpGrammar :: [(Var,Value)] -> ApegGrm -> APegSt ()
interpGrammar vs [] = return ()
interpGrammar vs grm@((ApegRule _ _ syn b):_) 
       = do envAlter (\_ -> M.fromList (f vs))
            interp b
            ws <- mapM evalExp syn
            varsSet (outs ws)
                                                                            
    where outs l = zip [ "_varOut" ++ (show i) | i <- [1..length syn] ] l
          f [] = [("g",VLan grm)]
          f xs@((_,VLan _):_) = xs
          f xs = ("g",VLan grm):xs

testGrammarWithArgs :: ApegGrm -> [(Var,Value)] -> String -> ApegTuple
testGrammarWithArgs g vs s = snd $ runState (interpGrammar vs g) (zeroSt g s)
    
simpleTestWithArgs :: ApegGrm -> [(Var,Value)] -> String -> SmallTuple
simpleTestWithArgs g vs s = sel $ runState (interpGrammar vs g) (zeroSt g s)
    where sel (_,(g,e,t,r,i,k)) = (rmG e,r,i,k)
          rmG = M.delete "g"
    
