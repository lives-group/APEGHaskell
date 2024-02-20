module APEG.TypeSystem where


import APEG.Interpreter.MonadicState
import APEG.Interpreter.State
import APEG.AbstractSyntax
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State.Lazy

import Debug.Trace

type TYResult = Either [String] Type


tyr2list :: [TYResult] -> [Type]
tyr2list xs = [ t | (Right t) <- xs]

tyFail :: String -> TYResult
tyFail s = Left [s]

(>|<) :: String -> TYResult -> TYResult
(>|<) x (Left xs) = Left (x:xs)
(>|<) x r@(Right _) = r

(>||) :: String -> TYResult -> TYResult
(>||) x (Left xs) = Left (x:xs)
(>||) x r@(Right _) = Left [x]

tyClsc :: TYResult -> TYResult -> TYResult
tyClsc (Left xs) (Left ys) = Left (xs ++ ys)
tyClsc l@(Left xs) (Right _) = l
tyClsc (Right t) l@(Left ys) =l
tyClsc (Right t) (Right t') 
    | t == t'   = Right t
    | otherwise = Left ["type mismatch: " ++ (show t) ++ " and " ++ (show t')]
    
tyClscR :: Type -> TYResult -> TYResult -> TYResult
tyClscR _ (Left xs) (Left zs)   = Left (xs ++ zs)
tyClscR _ l@(Left xs) (Right _) = l
tyClscR _ (Right _) l@(Left zs) = l
tyClscR r (Right _) (Right _) = (Right r)

coalesceErr :: [TYResult] -> TYResult
coalesceErr xs = Left $ concat [ s | (Left s)<- xs]

matchAll :: [Type] -> [Type] -> Bool
matchAll xs ys = (and $ zipWith (==) xs ys) && ((length xs) == (length ys))

binPegTyInfer :: String -> String -> TYResult -> TYResult -> APegSt (TYResult)
binPegTyInfer op term r1 r2 = return $ (tyClscR TyAPeg r1 r2)

unPegTyInfer :: String -> String -> TYResult -> APegSt (TYResult)
unPegTyInfer _ _ r@(Right TyAPeg) = return r
unPegTyInfer op term l@(Left msg)  = return l

checkMapIns :: TYResult -> TYResult -> TYResult -> TYResult
checkMapIns (Right t@(TyMap m)) (Right key) (Right val)
    | (m == val) && key == TyStr = pure t
    | otherwise = tyFail ("Incompatible types at map acces: " ++ pprintType t ++ " and " ++ pprintType key)
checkMapIns _ _ _ = tyFail "Incosistent map acces call. (Hint: are the values real map and key ?)" 
   
-- safeVar :: NonTerminal -> Var -> Type -> APegSt

validateRuleExt :: TYResult -> TYResult -> TYResult -> TYResult
validateRuleExt (Right TyLanguage) (Right TyStr) (Right TyMetaAPeg) = pure TyLanguage
validateRuleExt  l s m =   coalesceErr [(s1 >|< l),  (s2 >|< s), (s3 >|< m), tyFail s4]
    where s1 = "Extension targeg isnt't a langauge"
          s2 = "Name grammar expression isn't String"
          s3 = "Grammar body type is not MetaPeg !"
          s4 = "Improper types for language extension"


validateRuleCreate :: TYResult -> [(TYResult,TYResult)] -> [TYResult] -> TYResult -> TYResult
validateRuleCreate (Right TyStr) xs rs (Right TyMetaAPeg) 
   | (all (== (Right TyMetaType, Right TyStr)) xs) && (all isMetaType (tyr2list rs)) = Right TyGrammar  --Right (TyRule (tyr2list $ map fst xs) (tyr2list rs))
   | otherwise = tyFail "Ill typed rule creation command."
validateRuleCreate (Right TyStr) _ _ (Right _) = tyFail "Illegal body for rule creation"
validateRuleCreate (Right _) _ _ (Right TyMetaAPeg) = tyFail "Illegal type name for rule creation"  
validateRuleCreate (Left m) _ _ _ = Left m  
 
 
varDecChk :: NonTerminal -> Var -> TYResult -> APegSt TYResult
varDecChk nt v l@(Left _) = return l
varDecChk nt v (Right t) = do mt <- varTypeOn nt v
                              maybe (recordVarOn nt v t >> (return $ pure TyAPeg) )
                                    (\t' ->  if t==t' then return $ pure TyAPeg else return $ tyFail ("Illegal type bind of " ++ (show v) ++ " with type " ++ ( show t )++  " and type " ++ (show t') ++ " at rule " ++ nt) ) 
                                    mt
                                   
 
tyUpdate :: NonTerminal -> (Var,Expr) -> APegSt TYResult
tyUpdate nt b@(v,e) = do tyr <- inferTypeExpr nt e 
                         ftyr <- varDecChk nt v tyr
                         return ((" >> Type error at bind of " ++ (show v) ++ " to " ++ (show e))  >|< ftyr)  
                         

assureTy :: (Type-> Bool) -> String -> TYResult -> TYResult
assureTy p s (Right t)  
    | p t = Right t
    | otherwise = tyFail s 
assureTy p s x = x 

assureTyWith :: (Type-> Bool) -> String -> Type -> TYResult -> TYResult
assureTyWith p s r (Right t)
    | p t = Right r
    | otherwise = tyFail s
assureTyWith _ s _ (Left xs) = Left (s:xs)

typeInheritedArg :: NonTerminal -> [Var] -> [Type] -> APegSt TYResult
typeInheritedArg nt xs ys = zs >>= return.(foldr tyClsc (pure TyAPeg)) 
    where chk = uncurry (varDecChk nt) 
          zs = mapM chk (zip xs (map pure ys))

mapPair :: Monad m => (a -> m c) -> (b -> m d) -> (a,b) -> m (c,d)
mapPair f g (a,b) = do x <- f a
                       y <- g b
                       return (x,y)
                                                          
                                                                       
-- 0 menas plus 
checkBinOp :: String -> Int -> Type -> Type ->  APegSt (TYResult)
checkBinOp nt 0 TyInt TyInt = return $ pure TyInt  
checkBinOp nt 0 _ _ =   return $ tyFail ("Plus operation type mismatch "++ nt)

inferTypeExpr :: NonTerminal -> Expr -> APegSt (TYResult)
inferTypeExpr nt (Str _)  = return $ pure TyStr
inferTypeExpr nt (ILit i)  = return $ pure TyInt
inferTypeExpr nt (Epsilon) = return $ pure TyGrammar
inferTypeExpr nt (EVar s) = varTypeOn nt s >>= maybe (return $ tyFail ("Undeclared variable: " ++ s ++ " at rule " ++ nt)) (return.pure)
inferTypeExpr nt (BinOp cod l r) = do tl <- inferTypeExpr nt l
                                      tr <- inferTypeExpr nt r
                                      case (tl,tr) of
                                           (Right tyl, Right tyr) -> checkBinOp nt cod tyl tyr
                                           (errl@(Left _),_) -> return errl
                                           (_,errr@(Left _))  -> return errr
inferTypeExpr nt (Union e d) = do  t1 <- inferTypeExpr nt e
                                   t2 <- inferTypeExpr nt d
                                   case (t1,t2) of
                                             (Right TyLanguage,Right TyGrammar) -> return $ pure TyLanguage
                                             (Right TyGrammar,Right TyGrammar) -> return $ pure TyGrammar
                                             _  -> return $ tyFail ("Illegal union of " ++ (show e) ++ (show d))
inferTypeExpr nt (MapLit []) = return $ tyFail (" Untypeable map literal at rule " ++ nt) 
inferTypeExpr nt (MapLit xs) = do ts <- mapM ((inferTypeExpr nt).snd) xs
                                  tk <- mapM ((inferTypeExpr nt).fst) xs
                                  case (all (== (head ts)) ts, all (== Right TyStr) tk) of
                                      (True,True) -> return $ pure $ TyMap (head (tyr2list ts)) 
                                      (False,_ ) -> return $ tyFail ("All exps in Map must have the same type " ++ show xs ++ " at rule " ++ nt)
                                      (_,False ) -> return $ tyFail ("All keys in Map must have the type string " ++ show xs ++ " at rule " ++ nt)
inferTypeExpr nt (MapIns e k ex) = do tm <- inferTypeExpr nt e
                                      tk <- inferTypeExpr nt k
                                      tv <- inferTypeExpr nt ex
                                      return $ ("Illegal map insertion: map " ++ (show e) ++ " and valule " ++ (show ex)) >|< (checkMapIns tm tk tv) 
inferTypeExpr nt (MapAccess m k) = do tm <- inferTypeExpr nt m
                                      tk <- inferTypeExpr nt k
                                      case (tm, tk) of
                                        (Right (TyMap t), Right TyStr) -> return $ pure t
                                        _   -> return $ tyFail ("Illegal map access: " ++ (show m) ++ " " ++ (show k))
inferTypeExpr nt r@(ExtRule grm rname mapeg) = do tylam  <- inferTypeExpr nt grm 
                                                  tystr  <- inferTypeExpr nt rname
                                                  tympeg <- inferTypeExpr nt mapeg
                                                  return $ ("Illegal rule extension at " ++ show r) >|< (validateRuleExt tylam tystr tympeg)
inferTypeExpr nt r@(MkRule grm inh syn mapeg)
    = do tylam <- inferTypeExpr nt grm
         -- Meta-Dyn Checking must be done with rule context !
         results <- mapM ((inferTypeExpr nt).snd) syn
         tympeg  <- inferTypeExpr nt mapeg
         tyinh <- mapM (\(et,ee) -> inferTypeExpr nt et >>= (\tet -> inferTypeExpr nt ee >>= (\tee -> return (tet,tee) )) ) inh
         result <- return $ ("Illegal meta rule creation at : " ++ nt) >|< (validateRuleCreate tylam tyinh results tympeg)
         return  result 

inferTypeExpr nt (MetaPeg mpeg) = inferTypeMpeg nt mpeg 
inferTypeExpr nt (MetaExp mexp) = inferTypeMExpr nt mexp

onType :: Type -> TYResult -> APegSt a -> APegSt a -> APegSt a
onType tref (Right t) suc err
     | tref == t = suc
     | otherwise = err
onType tref (Left m) suc err = err 

metaTypeError :: MAPeg -> TYResult -> APegSt TYResult
metaTypeError ma t@(Right t') = return $ ("Incompatible types at metaExpression " ++ (show ma) ++ ". It's argument should have type MetaAPeg, but has type" ++ (show t')) >|| t 
metaTypeError ma t@(Left t') = return $ Left (t' ++ [("Incompatible types at metaExpression " ++ (show ma) )])

inferTypeMpeg :: NonTerminal -> MAPeg -> APegSt TYResult
inferTypeMpeg nt MkLambda = return $ pure TyMetaAPeg
inferTypeMpeg nt mp@(MkLit e) = inferTypeExpr nt e >>= return.(assureTyWith (==TyStr ) ("Meta Literal\'" ++ show e ++ "\' error at rule " ++ nt) TyMetaAPeg)  
inferTypeMpeg nt mp@(MkCal nte xs ys) = do tnt <- inferTypeExpr nt  nte 
                                           txs <- mapM (inferTypeExpr nt) xs   
                                           tys <- mapM (inferTypeExpr nt) ys 
                                           onType TyStr
                                                  tnt
                                                  (if (all isMetaType (tyr2list (txs ++ tys))) 
                                                       then return $ pure TyMetaAPeg 
                                                       else return $ tyFail ("All arguments of " ++ (show mp) ++ " must have TyMetaExp type"))  
                                                  (return $ tyFail ("The expression " ++ (show nte) ++ " should have type TyStr")) 
inferTypeMpeg nt mp@(MkKle e) = inferTypeExpr nt e >>= \t -> onType TyMetaAPeg t (return  t) (metaTypeError mp t)
inferTypeMpeg nt mp@(MkNot e) = inferTypeExpr nt e >>= \t -> onType TyMetaAPeg t (return  t) (metaTypeError mp t)
inferTypeMpeg nt mp@(MkConstr c b) = do tyMc <- inferTypeExpr nt c  
                                        tyBody <- inferTypeExpr nt b
                                        onType TyMetaExp tyMc (onType TyMetaAPeg tyBody (return $ pure TyMetaAPeg) (metaTypeError mp tyBody)) 
                                                              (metaTypeError mp tyMc)

inferTypeMpeg nt mp@(MkSeq ee ed) 
    = do te <- inferTypeExpr nt ee 
         td <- inferTypeExpr nt ed
         case (te,td) of
              (Right te, Right td') -> return $ pure TyMetaAPeg
              (Left  xs, Left  ys)  -> return (Left  (xs++ ys))
              (Left  xs, _)         -> return $ Left xs
              (_ , Left  xs)         -> return $ Left xs
inferTypeMpeg nt mp@(MkAlt ee ed) 
    = do te <- inferTypeExpr nt ee 
         td <- inferTypeExpr nt ed
         case (te,td) of
              (Right te, Right td') -> return $ pure TyMetaAPeg
              (Left  xs, Left  ys)  -> return (Left  (xs++ ys))
              (Left  xs, _)         -> return $ Left xs
              (_ , Left  xs)        -> return $ Left xs
inferTypeMpeg nt mp@(MkAE xs) = do ys <- mapM (mapPair (inferTypeExpr nt) (inferTypeExpr nt)) xs
                                   case (all (== (Right TyStr, Right TyMetaExp)) ys) of
                                         True -> return $ pure TyMetaAPeg
                                         False -> return $ tyFail ("All meta expressions updates must have type form String = Exp at rule " ++ nt)
                                        
                                        
                                        
inferTypeMExpr :: NonTerminal -> MExpr -> APegSt TYResult
inferTypeMExpr nt MEpsilon     = return $ pure TyMetaExp
inferTypeMExpr nt MkTyStr      = return $ pure TyMetaType
inferTypeMExpr nt MkTyInt      = return $ pure TyMetaType
inferTypeMExpr nt MkTyLanguage = return $ pure TyMetaType
inferTypeMExpr nt MkTyGrammar  = return $ pure TyMetaType
inferTypeMExpr nt (MkTyMap m)  = inferTypeExpr nt m >>= return.(assureTy (==TyMetaType) ("Meta map must have a Meta type as argument at rule " ++ nt))
inferTypeMExpr nt (MVar e)     = inferTypeExpr nt e >>= return.(assureTyWith (==TyStr) ("Meta Variable expression must reduce to String type at rule" ++ nt) TyMetaExp)
inferTypeMExpr nt (MILit e)    = inferTypeExpr nt e >>= return.(assureTyWith (==TyInt) ("Meta Variable expression must reduce to Int type at rule" ++ nt) TyMetaExp)
inferTypeMExpr nt (MStr e)     = inferTypeExpr nt e >>= return.(assureTyWith (==TyStr) ("Meta Variable expression must reduce to String type at rule" ++ nt) TyMetaExp)
inferTypeMExpr nt (MUnion e1 e2) = do  t1 <- inferTypeExpr nt e1
                                       t2 <- inferTypeExpr nt e2
                                       return $ ("Meta uniont, at rule " ++ nt) >|< (foldr1 tyClsc [pure TyMetaExp,t1,t2])
                                       
inferTypeMExpr nt (MMapLit xs)       = do ys <- mapM (mapPair (inferTypeExpr nt) (inferTypeExpr nt)) xs 
                                          return $ ("Meta map construction, at rule "++ nt) >|< (foldr1 tyClsc ((pure TyMetaExp):map (uncurry tyClsc) ys))
inferTypeMExpr nt (MMapIns em ek ev) = do tm <- inferTypeExpr nt em
                                          tk <- inferTypeExpr nt ek
                                          tv <- inferTypeExpr nt ev
                                          return $ ("Meta map construction, at rule "++ nt) >|< (foldr1 tyClsc [pure TyMetaExp,tm,tk,tv])
inferTypeMExpr nt (MMapAcces em ek) = do tm <- inferTypeExpr nt em
                                         tk <- inferTypeExpr nt ek
                                         return $ ("Meta map access, at rule " ++ nt) >|< ((foldr1 tyClsc [pure TyMetaExp,tm,tk])) 

inferPegType :: NonTerminal -> APeg -> APegSt TYResult
inferPegType nt Lambda   = return $ pure TyAPeg
inferPegType nt (Lit _)  = return $ pure TyAPeg
inferPegType nt r@(NT nt' inh syn)
   = do mnt <- ntType nt'
        maybe (return $ tyFail ("Undefined non-terminal: " ++ nt' ++ " at " ++ show r))
              (\(TyRule inh' syn') -> do inhTys <- mapM (inferTypeExpr nt) inh
                                         synTys <- typeInheritedArg nt syn syn'
                                         case ((matchAll (tyr2list inhTys) inh') && ( synTys == Right TyAPeg) ) of
                                            True  -> return $ pure TyAPeg
                                            False -> return $ tyFail ("Illegal rule call " ++ (show r) ++" at rule " ++ nt)) mnt

inferPegType nt p@(Kle e) = inferPegType nt e >>= unPegTyInfer "Klenee" nt
inferPegType nt p@(Not e) = inferPegType nt e >>= unPegTyInfer "Not" nt
inferPegType nt p@(Constr e a) = do c <- inferTypeExpr nt e  
                                    case c of 
                                         (Right TyBool) -> inferPegType  nt a
                                         (Right _) -> return $ tyFail ("Constraint expression must have boolean type at rule " ++ nt)
                                         e         -> return e

inferPegType nt p@(Seq e d) = do tye <- inferPegType nt e 
                                 tyd <- inferPegType nt d
                                 binPegTyInfer "Seq" nt tye tyd 
                                
inferPegType nt p@(Alt e d) = do renv  <- localRuleEnv nt
                                 tye   <- inferPegType nt e 
                                 renv' <- tyRuleEnvSwap nt renv
                                 tyd   <- inferPegType nt d
                                 renv'' <- localRuleEnv nt
                                 tr    <- binPegTyInfer "Alt" nt tye tyd
                                 tyRuleEnvSwap nt (M.intersection renv' renv'')
                                 return tr
                                 
inferPegType nt p@(Update xs)  =  (mapM (tyUpdate nt) xs) >>= return.(foldr1 tyClsc)
inferPegType nt p@(Bind v peg) = do tyv <- varTypeOn nt v
                                    typ <- inferPegType nt peg
                                    case (tyv,typ) of
                                         (Just TyStr, Right TyAPeg) -> return $ pure TyAPeg
                                         (Nothing, Right TyAPeg)    -> recordVarOn nt v TyStr >> return (pure TyAPeg) 
                                         (_, err@(Left _))          -> return err 
                                         _ -> return $ tyFail ("Illegal APEG bind " ++ (show p) ++ " at rule " ++ show nt) 
 


checkRuleDef :: [Type] -> [Type] -> ApegRule -> APegSt (TYResult)
checkRuleDef inh syn r@(ApegRule nt inh' syn' peg) 
    | matchAll (map fst inh') inh = do tb <- inferPegType nt peg
                                       innerEnv <- localRuleEnv nt
                                       ts <- mapM (inferTypeExpr nt) (map snd syn')
                                       if (matchAll (tyr2list ts) (map fst syn') ) then  return $ (assureTyWith (==TyAPeg)  ("Rule body " ++ nt ++ " does not type") (TyRule inh syn)  tb)
                                                                                   else  return $ tyFail ("Divergent returns types at rule " ++ nt)
    | otherwise = return  $ tyFail  ("Divergent argument list definitions of Rule " ++ nt)


inferTypeRule :: ApegRule -> APegSt (TYResult) 
inferTypeRule r@(ApegRule nt inh syn peg) 
     = do ntTy <- ntType nt
          case ntTy of
              Just t@(TyRule inh' syn') -> checkRuleDef inh' syn' r
              Nothing  -> do env'<- getTyEnv
                             r1 <- overlapTyEnv (M.union env' (tyEnvFromRule r))
                                                (do inferPegType nt peg
                                                    ts <- mapM (inferTypeExpr nt) (map snd syn) 
                                                    mty <- ruleEnv nt
                                                    return $ fmap (\(_,rEnv)  -> (TyRule (map fst inh) (tyr2list ts),rEnv)) mty) 
                             maybe (return $ tyFail ("Untypeable body at ")) (\r'@(tyr,tye) -> tyEnvAlter (M.insert nt r') >> return (pure tyr)) r1


typeGrammar :: ApegGrm -> APegSt ([String])
typeGrammar grm = do xs <- mapM inferTypeRule  grm
                     return ([ unlines zs | (Left zs) <- xs ])


simpleType :: ApegGrm -> [String]
simpleType grm = fst $ runState (typeGrammar grm) (zeroSt grm "")

simpleTypeEnv :: ApegGrm -> TyEnv
simpleTypeEnv grm = tyEnv $ snd (runState (typeGrammar grm) (zeroSt grm ""))

showTyEnv :: ApegGrm -> IO ()
showTyEnv g = putStrLn (pprintTyEnv $ simpleTypeEnv g)

