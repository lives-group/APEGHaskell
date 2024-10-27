module APEG.Interpreter.APEGInterp where
       
import APEG.AbstractSyntax
import APEG.Interpreter.MonadicState
import APEG.Interpreter.State 
import APEG.Interpreter.Value
import APEG.Combinators
import APEG.Interpreter.DT
import APEG.Interpreter.MaybeString
import qualified Data.Map as M
import Control.Monad.State.Lazy
import APEG.TypeSystem
import Debug.Trace


callNt :: String -> [Value] -> [Var] -> APegSt ()
callNt nt vs@((VLan g ty):inh) vars
   = case fetch g nt of
            Just r  -> tyEnvAlter (\_ -> ty) >> dtRuleBuild (interpRule vs vars r >> modify (rootr nt)) 
            Nothing -> getTyEnv >>= \tyenv -> error ("Attempt to call non existent rule: " ++ nt ++ "\n\n" ++ (pprintTyEnv tyenv))
callNt _ _ _ = error "Panic !, Missing language attribute !" 


interpRule :: [Value] -> [Var] -> ApegRule -> APegSt ()
interpRule vs os (ApegRule nt inh syn b) 
     = do zs <- overlapEnv (envFromDec inh vs) 
                           (interp b >> 
                            onSuccess (mapM (evalExp.snd) syn >>= \ws -> return $ zip os ws)
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
dynRule (VLan grm ty) (VStr nt) (VPeg p) = do g' <- return $ grmExtRule grm nt p
                                              st <- get
                                              errs <- typeGrammar g'
                                              t' <- getTyEnv
                                              put st
                                              return $ vlan t' g'
dynRule _ _ _ = error "Unproper attempt to compose an existing rule !"


-- =================== Monad Utilities =================== --

-- | Extends the current grammar g with another give grammar g'. The rules 
-- of g'will be appended at the grammar g
langExt :: ApegGrm -> APegSt ()
langExt grm 
     =  do (localgrm, localty) <- getLanguage
           grm' <- return $ joinRules localgrm grm
           errs <- typeGrammar grm'
           t' <- getTyEnv
           case errs of
                [] ->  modify (\pst -> upValEnv (M.update (\oldg -> Just $ vlan t' grm') "g") pst)
                xs@(_:_) -> error $ unlines ("------- Dynamic type error -------": xs ++ ["--- Check your meta rules ---"])

composeLang :: (ApegGrm,TyEnv) -> ApegGrm -> APegSt (ApegGrm, TyEnv)
composeLang (g,t) g' = do st <- get
                          grm' <- return $ joinRules g g'
                          tyEnvAlter (\ _ -> tyEnvFromGrm grm')
                          --trace (show grm') (return ())
                          errs <- typeGrammar grm'
                          t' <- getTyEnv
                          put st
                          case errs of
                              []       -> return (grm',t') 
                              xs@(_:_) -> error $ unlines ("\n------- DYNAMIC TYPE ERROR -------": xs ++ ["----- Check your meta rules -----"])

lanUnion :: Value -> Value -> APegSt Value
lanUnion (VGrm l1) (VLan l2 ty2) = composeLang (l2,ty2) l1 >>= (\(g,t) -> return $ vlan t g)
lanUnion (VLan l1 ty1) (VGrm l2) = composeLang (l1,ty1) l2 >>= (\(g,t) -> return $ vlan t g)
lanUnion (VGrm l1) (VGrm l2)     = return (VGrm $ joinRules l1 l2 )
lanUnion  l1 l2 = error ("Attempt to unite two values that aren't languages " ++ (show l1) ++ " and " ++ (show l2))


unMeta :: MAPeg -> APegSt (APeg)
unMeta MkLambda           = return $ Lambda
unMeta (MkLit e)          = evalExp e >>= return.(Lit).strVal
unMeta (MkCal nt inh syn) = do ntv <- evalExp nt 
                               xs <- mapM evalExp inh 
                               ys <- mapM evalExp syn
                               return $ NT (strVal ntv) (map expFromVal xs) (map varNameFromVal ys)
unMeta (MkKle e)          = evalExp e >>= return.(Kle).apegFromVal
unMeta (MkNot e)          = evalExp e >>= return.(Not).apegFromVal
unMeta (MkConstr t b)     = do et <- evalExp t 
                               ab <- evalExp b 
                               return $ Constr (expFromVal et) (apegFromVal ab)
unMeta (MkSeq x y)        = evalExp x >>= \px -> evalExp y >>= \py -> return $ Seq (apegFromVal px) ((apegFromVal py))
unMeta (MkAlt x y)        = evalExp x >>= \px -> evalExp y >>= \py -> return $ Alt ((apegFromVal px)) (apegFromVal py)
unMeta (MkAE xs)          = do ys <- mapM (\(ev,ee) -> do r <- evalExp ee
                                                          v <- evalExp ev
                                                          return (strVal v,expFromVal r)) xs
                               return $ Update ys
                               
                               
unMetaExp :: MExpr -> APegSt (Expr)
unMetaExp MEpsilon         = return $ Epsilon
unMetaExp (MVar e)         = evalExp e >>= return.(EVar).strVal
unMetaExp (MStr e)         = evalExp e >>= return.(Str).strVal 
unMetaExp (MILit e)        = evalExp e >>= return.(ILit).intVal 
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
unMetaExp MkTyInt           = return $ MetaExp MkTyInt
unMetaExp MkTyGrammar       = return $ MetaExp MkTyGrammar
unMetaExp MkTyLanguage      = return $ MetaExp MkTyLanguage



mapInsert :: Value -> String -> Value -> APegSt (Value)
mapInsert (VMap m) s v = return $ VMap (M.insert s v m)
mapinsert v _ _        = fail (" value: " ++ (show v) ++ " is not a map.") 

mapAcces :: Value -> Value -> APegSt (Value)
mapAcces (VMap m) (VStr s) = case (m M.!? s) of
                                 Just r -> return r
                                 Nothing -> return Undefined
                                 
mapAccess m      (VStr _)  = fail (" value: " ++ (show m) ++ " is not a map.") 
mapAccess m  x  = fail (" value: " ++ (show x) ++ " is not a string.") 


-- 0 = +
-- 1 = -
-- 2 = <
-- 3 = =
evalBinOp :: Int -> Value -> Value -> APegSt Value
evalBinOp 0 (VInt x) (VInt y) = return (VInt $ x+y)
evalBinOp 1 (VInt x) (VInt y) = return (VInt $ x-y)
evalBinOp 2 (VInt x) (VInt y) = return  (VBool $ x < y)
evalBinOp 3 (VInt x) (VInt y) = return (VBool $ x == y)
evalBinOp 3 (VBool x) (VBool y) = return (VBool $ x == y)

evalExp :: Expr -> APegSt (Value)
evalExp (Epsilon) = return (VGrm [])
evalExp (Str s) = return (VStr s)
evalExp (ILit i) = return (VInt i)
evalExp (EVar v)   = var v
evalExp (BinOp cod l r) = do ve <- evalExp l
                             vd <- evalExp r
                             evalBinOp cod ve vd
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
evalExp (MetaExp MkTyGrammar) = return $ vtype TyGrammar 
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
interp (NT s inh ret) = do inh' <- (mapM evalExp inh)
                           callNt s inh' ret 
--interp (Kle p)        = klenne (interp p) >> return ()
interp (Kle p)        = klenne (interp p)
interp (Seq e d)      = sequential (interp e) (interp d)
interp (Alt e d)      = alternate (interp e) (interp d)
interp (Not e)        = notPeg (interp e)
interp (Constr e p)   = evalExp e >>= \b -> constraintApeg b (interp p)
interp (Update xs)    = mapM_ (\(v,e) -> update v (evalExp e)) xs
interp (Bind v p)     = bindApeg v (interp p)
interp (Ann Lift p@(NT _ _ _))   = do r <- result
                                      modifyResult  (Right [])
                                      interp p
                                      mapResult liftvar
                                      r' <- result
                                      modifyResult  (catResult r r')
interp (Ann Flat p)   = do r <- swapResult (Right [])
                           st <- get
                           interp p
                           mapResult (\xs -> [ dtLeaf (valEnv st) (concatMap dtFlat xs)])
                           prependResult r
interp (Ann Dump p)   = do r <- swapResult (Right [])
                           interp p
                           mapResult (\_ -> [])
                           prependResult r

liftvar :: [DTV] -> [DTV]
liftvar [] = []
liftvar [x] = liftNT x
liftvar (x:xs) =  x : liftvar xs

execGrm :: TyEnv -> [(Var,Value)] -> ApegGrm ->  APegSt ()
execGrm ty vs grm@((ApegRule nt _ syn b):_)
    = do envAlter (\_ -> M.fromList (f vs))
         interp b
         modify (rootr nt)
         onSuccess (mapM (evalExp.snd) syn >>= \ws -> varsSet (outs ws))
                   (return ())
    where outs l = zip [ "_varOut" ++ (show i) | i <- [1..length syn] ] l
          f [] = [("g",VLan grm ty)]
          f xs@((_,VLan _ _):_) = xs
          f xs = ("g",VLan grm ty):xs
                                           
interpGrammar :: [(Var,Value)] -> ApegGrm -> APegSt ()
interpGrammar vs [] = return ()
interpGrammar vs grm@((ApegRule nt _ syn b):_) 
       = do res <- typeGrammar grm
            ty <- getTyEnv
            case res of 
                 []       -> execGrm ty vs grm 
                 xs@(_:_) -> pfailMsg (unlines ("-------- Type errors found -------- " : xs ++ ["--Fix then before you try again !--"]))

{-
execGrammar :: ApegGrm -> [(Var,Value)] -> String -> PureState
execGrammar g vs s = snd $ runState (interpGrammar vs g) (zeroSt g s)
 -}   
-- simpleTestWithArgs :: ApegGrm -> [(Var,Value)] -> String -> (VEnv,MybStr,String,Result)
-- simpleTestWithArgs g vs s = sel $ runState (interpGrammar vs g) (zeroSt g s)
--     where sel (_,s) = (rmG (valEnv s),getPrefix s,remInp s,getResult s)
--           rmG = M.delete "g" 
{-     
simpleTestWithArgs :: ApegGrm -> [(Var,Value)] -> String -> (VEnv,MybStr,String,Result)
simpleTestWithArgs g vs s = sel $ runState (interpGrammar vs g) (zeroSt g s)
    where sel (_,s) = (rmG (valEnv s),getPrefix s,remInp s,getResult s)
          rmG = M.delete "g" 
          
booleanTest :: ApegGrm -> [(Var,Value)] -> String -> Bool
booleanTest g vs s = sel $ runState (interpGrammar vs g) (zeroSt g s)
    where sel (_,st) = case getResult st of
                         (Right _) -> True
                         (Left _) -> False
                           -}
