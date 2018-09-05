module APEGTypeSystem where


import APEGState
import AbstractSyntax
import Data.Maybe
import qualified Data.Map as M

binPegTyInfer :: String -> String -> Type -> Type -> APegSt (Type)
binPegTyInfer _ _ TyAPeg TyAPeg = return TyAPeg
binPegTyInfer op term _ _ = fail ("Illegal " ++ op ++ " construction at " ++ term) 

unPegTyInfer :: String -> String -> Type -> APegSt (Type)
unPegTyInfer _ _ TyAPeg = return TyAPeg
unPegTyInfer op term _  = fail ("Illegal " ++ op ++ " construction at " ++ term)

checkMapIns :: Type -> Type -> Type -> Maybe Type
checkMapIns t@(TyMap m) key val
    | (m == val) && key == TyStr = Just t
    | otherwise = Nothing

   
-- safeVar :: NonTerminal -> Var -> Type -> APegSt

validateRuleExt :: Type -> Type -> Type -> Maybe Type
validateRuleExt TyLanguage TyStr TyMetaAPeg = Just TyLanguage
validateRuleExt _ _ _  = Nothing

validateRuleCreate :: Type -> [(Type,Var)] -> [Type] -> Type -> Maybe Type
validateRuleCreate TyStr xs rs TyMetaAPeg 
   | (all isMetaType (map fst xs)) && (all isMetaType rs) = Just (TyRule (map fst xs) rs)
   | otherwise = Nothing
   
   
tyBind :: NonTerminal -> (Var,Expr) -> APegSt Type
tyBind nt b@(v,e) = do t <- inferTypeExpr nt e
                       mt <- varTypeOn nt v 
                       case mt of
                            Just t' -> if t==t' then return TyAPeg
                                                else bindError
                            Nothing -> recordVarOn nt v t >> return TyAPeg
  where
      bindError = fail ("Illegal bind of " ++ (show e) ++ " to " ++ v)
      
inferTypeExpr :: NonTerminal -> Expr -> APegSt (Type)
inferTypeExpr nt (Str _)  = return TyStr
inferTypeExpr nt (EVar s) = 
     varTypeOn nt s >>= maybe (fail ("Undeclared variable: " ++ s ++ " at rule " ++ nt)) (return)
                                  
inferTypeExpr nt (Union e d) = do  t1 <- inferTypeExpr nt e
                                   t2 <- inferTypeExpr nt d
                                   case (t1,t2) of
                                        (TyLanguage,TyLanguage) -> return TyLanguage
                                        _  -> fail ("Illegal union of " ++ (show e) ++ (show d))
inferTypeExpr nt (MpLit []) = fail " Untypeable map ! "
inferTypeExpr nt (MpLit xs) = do ts <- mapM ((inferTypeExpr nt).snd) xs
                                 case all (== (head ts)) ts of
                                      True -> return (head ts) 
                                      False -> fail ("All exps in Map must have the same type" ++ show xs)
                                      
inferTypeExpr nt (MapIns e k ex) = do tm <- inferTypeExpr nt e
                                      tk <- inferTypeExpr nt k
                                      tv <- inferTypeExpr nt ex
                                      case checkMapIns tm tk tv of
                                           Just t -> return t
                                           Nothing -> fail ("Illegal map insertion: map " ++ (show tm) ++ "and valule " ++ (show tv))
inferTypeExpr nt (MapAccess m k) = do tm <- inferTypeExpr nt m
                                      tk <- inferTypeExpr nt k
                                      case (tm, tk) of
                                        (TyMap t, TyStr) -> return t
                                        _   -> fail ("Illegal map access: " ++ (show m) ++ " " ++ (show k))
inferTypeExpr nt r@(ExtRule grm rname mapeg) = do tylam  <- inferTypeExpr nt grm 
                                                  tystr  <- inferTypeExpr nt rname
                                                  tympeg <- inferTypeExpr nt mapeg
                                                  maybe (fail ("Ilegal rule extension at " ++ show r))
                                                        (return)
                                                        (validateRuleExt tylam tystr tympeg)                                                    
inferTypeExpr nt r@(MkRule grm inh syn mapeg)
    = do tylam <- inferTypeExpr nt grm
         -- Meta-Dyn Checking must be done with rule context !
         results <- mapM ((inferTypeExpr nt).snd) syn
         tympeg  <- inferTypeExpr nt mapeg
         case validateRuleCreate tylam inh results tympeg of
            Just t -> return t
            Nothing -> fail ("Illegal rule extension at : " ++ show r)

inferTypeExpr nt (MetaPeg mpeg) = undefined -- inferTypeMpeg nt mpeg 
inferTypeExpr nt (MetaExp mexp) = undefined -- inferTypeMExpr nt mexp

inferPegType :: NonTerminal -> APeg -> APegSt Type
inferPegType nt Lambda   = return TyAPeg
inferPegType nt (Lit _)  = return TyAPeg
inferPegType nt r@(NT nt' inh syn)
   = do mnt <- ntType nt'
        maybe (fail ("Undefined non-terminal: " ++ nt' ++ " at " ++ show r))
              (\(TyRule inh' syn') -> do inhTys <- mapM (inferTypeExpr nt) inh
                                         synTys <- mapM (\var ->varTypeOn nt var >>= return.fromJust) syn
                                         case ((matchAll inhTys inh') && (matchAll syn' synTys)) of
                                            True  -> return TyAPeg
                                            False -> fail ("Illegal rule call " ++ (show r))) mnt
inferPegType nt p@(Kle e) = inferPegType nt e >>= unPegTyInfer "Kle" (show p)
inferPegType nt p@(Not e) = inferPegType nt e >>= unPegTyInfer "Not" (show p)
inferPegType nt p@(Seq e d) = do tye <- inferPegType nt e 
                                 tyd <- inferPegType nt d
                                 binPegTyInfer "Seq" (show p) tye tyd 
                                 
inferPegType nt p@(Alt e d) = do tye <- inferPegType nt e 
                                 tyd <- inferPegType nt d
                                 binPegTyInfer "Alt" (show p) tye tyd
                                 
inferPegType nt p@(AEAttr xs)  = mapM_ (tyBind nt) xs >> return TyAPeg
inferPegType nt p@(Bind v peg) = do tyv <- varTypeOn nt v
                                    typ <- inferPegType nt peg
                                    case (tyv,typ) of
                                         (Just TyStr, TyAPeg) -> return TyAPeg
                                         (Nothing, TyAPeg) -> recordVarOn nt v TyStr >> return TyAPeg 
                                         _ -> fail ("Illegal APEG bind at " ++ show p) 
 
-- checkRuleResult :: NonTerminal -> [(Type,Expr)] -> APegSt ()
-- checkRuleResult nt outs syn' = do ts <- mapM (inferTypeExpr nt) outs
--                                   f (matchAll ts syn')  
--     where f True = return ()
--           f False = fail ("Rule " ++ nt ++ " results diverge from the rule's type in context ")

checkRuleDef :: [Type] -> [Type] -> ApegRule -> APegSt ()
checkRuleDef inh syn r@(ApegRule nt inh' syn' peg) 
    | matchAll (map fst inh') inh = do inferPegType nt peg 
                                       ts <- mapM (inferTypeExpr nt) (map snd syn')
                                       res $ and (zipWith (==) ts (map fst syn'))
    | otherwise = fail ("Multiple divergent definitions of Rule " ++ nt ++ "diverge.")
    where res True = return ()
          res False  = fail ("Type mismatch in results of " ++ show r)  


inferTypeRule :: ApegRule -> APegSt (Type) 
inferTypeRule r@(ApegRule nt inh syn peg) 
     = do ntTy <- ntType nt
          case ntTy of
              Just t@(TyRule inh' syn') -> checkRuleDef inh' syn' r >> return t
              Nothing  -> do env'<- tyEnv
                             r <- supresTyEnv (M.union env' (tyEnvFromRule r))
                                              (do inferPegType nt peg
                                                  ts <- mapM (inferTypeExpr nt) (map snd syn) 
                                                  (Just (_,rEnv)) <- ruleEnv nt 
                                                  return (TyRule (map fst inh) ts,rEnv))
                             tyEnvAlter (M.insert nt r)
                             return $ fst r
                             
typeGrammar :: ApegGrm -> APegSt ()
typeGrammar = mapM_ inferTypeRule 
