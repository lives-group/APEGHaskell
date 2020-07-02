module APEG.Interpreter.APEGInterp where
       
import APEG.AbstractSyntax
import APEG.Interpreter.MonadicState
import APEG.Interpreter.State 
import APEG.Interpreter.Value
import APEG.Combinators
import APEG.TypeSystem
import APEG.Interpreter.MaybeString
import qualified Data.Map as M
import Control.Monad.State.Lazy

import Debug.Trace


bind :: (Var, Expr) -> APegSt ()
bind (v,e) = evalExp e >>= (varSet v) 

                                    
bindPeg :: Var -> APeg -> APegSt ()
bindPeg v p  = do prfx <- getStr
                  resetStr
                  interp p
                  res <- getStr
                  modify (prependPrefix prfx)
                  varSet v (vstr $ fromMybStr res)
                                     
callNt :: String -> [Value] -> [Var] -> APegSt ()
callNt nt vs@((VLan g):inh) vars
   = case fetch g nt of
            Just r  -> interpRule vs vars r 
            Nothing -> getTyEnv >>= \tyenv -> error ("Attempt to call non existent rule: " ++ nt ++ "\n\n" ++ (pprintTyEnv tyenv))
callNt _ _ _ = error "Panic !, Missing language attribute !" 

interpRule :: [Value] -> [Var] -> ApegRule -> APegSt ()
interpRule vs os (ApegRule nt inh syn b) 
     = do zs <- overlapEnv (envFromDec inh vs) 
                           (interp b >> 
                           onSucess (mapM (evalExp.snd) syn >>= \ws -> return $ zip os ws)
                                    (return []))
          varsSet zs

metaInhAttr2AST :: [(Value,Value)] -> [(Type,String)]
metaInhAttr2AST = map (\(x,y) -> (typeFromVal x, strVal y))

metaSynAttr2AST :: [(Value,Value)] -> [(Type,Expr)]
metaSynAttr2AST = map (\(x,y) -> (typeFromVal x, expFromVal y))

ruleCreate :: Value -> [(Value,Value)] -> [(Value,Value)] -> Value -> APegSt (Value)
ruleCreate (VStr s) inh syn b
  | (all (\(e,x) -> (valIsMExpr e) && (valIsMExpr x) ) syn) && (valIsMPeg b) = return $ VGrm [ApegRule s (metaInhAttr2AST inh) (metaSynAttr2AST syn) (apegFromVal b)]
  | otherwise = error "Unproper attempt to create a new rule: Rule parameters/returns incoccrectly constructed"
ruleCreate _ inh syn b = error "Unproper attempt to create a new rule: Wrong rule name"


dynRule :: Value -> Value -> Value -> APegSt Value
dynRule (VLan grm) (VStr nt) (VPeg p) = return $ VGrm (grmExtRule grm nt p)
dynRule _ _ _ = error "Unproper attempt to compose an existing rule !"


-- =================== Monad Utilities =================== --


lanUnion ::Value -> Value -> APegSt Value
--lanUnion (VLan l1) (VLan l2) = return (VLan $ joinRules l1 l2 )
lanUnion (VLan l1) (VGrm l2) = do xs <- typeGrammar l2
                                  case xs of
                                       []    -> return (VLan $ joinRules l1 l2 )
                                       (_:_) -> error ("Dynamic type error while type gramar extension. Type errors follow\n " ++ (unlines xs)) 
lanUnion (VGrm l1) (VGrm l2) = return (VGrm $ joinRules l1 l2 )
lanUnion _ _ = remStr >>= \s -> error ("Attempt to unite two values that aren't languages. Remaining input:\n" ++ s)

-- Unquote !
unMeta :: MAPeg -> APegSt (APeg)
unMeta MkLambda           = return $ Lambda
unMeta (MkLit e)          = evalExp e >>= return.(Lit).strVal
unMeta (MkCal nt inh syn) = do ntv <- evalExp nt 
                               xs <- mapM evalExp inh 
                               ys <- mapM evalExp syn
                               return $ NT (strVal ntv) (map expFromVal xs) (map varNameFromVal ys)
unMeta (MkKle e)          = evalExp e >>= return.(Kle).apegFromVal
unMeta (MkNot e)          = evalExp e >>= return.(Not).apegFromVal
unMeta (MkSeq x y)        = evalExp x >>= \px -> evalExp y >>= \py -> return $ Seq (apegFromVal px) ((apegFromVal py))
unMeta (MkAlt x y)        = evalExp x >>= \px -> evalExp y >>= \py -> return $ Alt ((apegFromVal px)) (apegFromVal py)
unMeta (MkAE xs)          = do ys <- mapM (\(ev,ee) -> do r <- evalExp ee
                                                          v <- evalExp ev
                                                          return (strVal v,expFromVal r)) xs
                               return $ AEAttr ys
                               
unMetaExp :: MExpr -> APegSt (Expr)
unMetaExp MEpsilon         = return $ Epsilon
unMetaExp (MVar e)         = evalExp e >>= return.(EVar).strVal
unMetaExp (MStr e)         = evalExp e >>= return.(Str).strVal 
unMetaExp (MUnion e1 e2)   = do ue1 <- evalExp e1
                                ue2 <- evalExp e2
                                return (Union (expFromVal ue1) (expFromVal ue2))
unMetaExp (MMapLit xs)     = do ys <- mapM (\(mk,mv) -> evalExp mk >>= \k -> evalExp mv >>= \v -> return (expFromVal k,expFromVal v)) xs
                                return (MapLit ys)
unMetaExp (MMapIns e1 e2 e3) = do ue1 <- evalExp e1 
                                  ue2 <- evalExp e2
                                  ue3 <- evalExp e3
                                  return (MapIns (expFromVal ue1)  (expFromVal ue2)  (expFromVal ue3))
unMetaExp (MMapAcces e1 e2)  = do ue1 <- evalExp e1 
                                  ue2 <- evalExp e2
                                  return (MapAccess (expFromVal ue1)  (expFromVal ue2))
unMetaExp m@(MkTyMap e)     = return $ MetaExp m
unMetaExp MkTyStr           = return $ MetaExp MkTyStr
unMetaExp MkTyLanguage      = return $ MetaExp MkTyLanguage



mapInsert :: Value -> String -> Value -> APegSt (Value)
mapInsert (VMap m) s v = return $ VMap (M.insert s v m)
mapInsert v _ _        = error (" value: " ++ (show v) ++ " is not a map.") 

mapAcces :: Value -> Value -> APegSt (Value)
mapAcces (VMap m) (VStr s) = case (m M.!? s) of
                                 Just r -> return r
                                 Nothing -> return Undefined
                                 
mapAccess m      (VStr _)  = pfailMsg (" value: " ++ (show m) ++ " is not a map.") 
mapAccess m  x  = pfailMsg (" value: " ++ (show x) ++ " is not a string.") 

evalExp :: Expr -> APegSt (Value)
evalExp (Epsilon) = return (VGrm [])
evalExp (Str s) = return (VStr s)
evalExp (EVar v)   = var v
evalExp (ExtRule lam ntexp mapeg) =  do grm <- evalExp lam
                                        nt <- evalExp ntexp
                                        apeg <- evalExp mapeg -- Dyn typing to be done here !
                                        dynRule grm nt apeg 
                                        
evalExp (MkRule nt inh syn b) = do ntName <- evalExp nt
                                   inh' <- mapM (\(vn,ex) -> evalExp vn >>= \vvn -> evalExp ex >>= \vvex -> return (vvn,vvex) ) inh
                                   syn' <- mapM (\(vn,ex) -> evalExp vn >>= \vvn -> evalExp ex >>= \vvex -> return (vvn,vvex) ) syn
                                   apeg <- evalExp b
                                   ruleCreate ntName inh' syn' apeg 
evalExp (Union e1 e2) = do l1 <- evalExp e1
                           l2 <- evalExp e2
                           lanUnion l1 l2
evalExp (MetaPeg m) = unMeta m >>=  return.VPeg
evalExp (MetaExp MkTyStr) = return $ vtype TyStr  
evalExp (MetaExp MkTyLanguage) = return $ vtype TyLanguage 
evalExp (MetaExp (MkTyMap mt)) = evalExp mt >>= return.vtype.(TyMap).typeFromVal 
evalExp (MetaExp m) = unMetaExp m >>= (return.vexpr)
evalExp (MapLit xs)  = (mapM (\(s,b) -> do vs <- evalExp s 
                                           vr <- evalExp b 
                                           return (strVal vs,vr)) xs) >>= (\xs -> return $ VMap (M.fromList xs))
evalExp (MapIns m s v)  = do mp  <- evalExp m
                             key <- evalExp s
                             val <- evalExp v
                             mapInsert mp (strVal key) val
evalExp (MapAccess m i) = do mp  <- evalExp m
                             str <- evalExp i
                             mapAcces mp str
                             

interp :: APeg -> APegSt ()
interp (Lambda)       =  done
interp (Lit s)        = patternMatch s
interp (NT s inh ret) = do inh' <- (mapM evalExp inh {->>= implicitLanArg-})
                           callNt s inh' ret 
interp (Kle p)        = klenne (interp p) >> return ()
interp (Seq e d)      = sequential (interp e) (interp d)
interp (Alt e d)      = alternate (interp e) (interp d)
interp (Not e)        = notPeg (interp e)
interp (AEAttr xs)    = mapM_ bind xs
interp (Bind v p)     = bindPeg v p

                                           
interpGrammar :: [(Var,Value)] -> ApegGrm -> APegSt ()
interpGrammar vs [] = return ()
interpGrammar vs grm@((ApegRule nt _ syn b):_) 
       = do envAlter (\_ -> M.fromList (f vs))
            interp b
            onSucess (mapM (evalExp.snd) syn >>= \ws -> varsSet (outs ws))
                     (get >>= \pst -> 
                               error ("Rule " ++ nt ++ " has failed. Remaining input \"" ++ (take 30 (remInp pst)) ++ "\".\n" ++ (show pst)))
                                                                            
    where outs l = zip [ "_varOut" ++ (show i) | i <- [1..length syn] ] l
          f [] = [("g",VLan grm)]
          f xs@((_,VLan _):_) = xs
          f xs = ("g",VLan grm):xs

testGrammarWithArgs :: ApegGrm -> [(Var,Value)] -> String -> PureState
testGrammarWithArgs g vs s = snd $ runState (interpGrammar vs g) (zeroSt g s)
    
simpleTestWithArgs :: ApegGrm -> [(Var,Value)] -> String -> (VEnv,MybStr,String,Result)
simpleTestWithArgs g vs s = sel $ runState (interpGrammar vs g) (zeroSt g s)
    where sel (_,s) = (rmG (valEnv s),getPrefix s,remInp s,getResult s)
          rmG = M.delete "g" 
     
