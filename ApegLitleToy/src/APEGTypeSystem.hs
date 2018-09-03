module APEGTypeSystem where


import APEGState
import AbstractSyntax
import Data.Maybe




checkMapIns :: Type -> Type -> Type -> Maybe Type
checkMapIns t@(TyMap m) key val
    | (m == val) && key == TyStr = Just t
    | otherwise = Nothing

validateRuleExt :: Type -> Type -> Type -> Maybe Type
validateRuleExt TyLanguage TyStr TyMetaAPeg = Just TyLanguage
validateRuleExt _ _ _  = Nothing

validateRuleCreate :: Type -> [(Type,Var)] -> [Type] -> Type -> Maybe Type
validateRuleCreate TyStr xs rs TyMetaAPeg 
   | (all isMetaType (map fst xs)) && (all isMetaType rs) = Just (TyRule (map fst xs) rs)
   | otherwise = Nothing


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
         results <- mapM (inferTypeExpr nt) syn
         tympeg <- inferTypeExpr nt mapeg
         case validateRuleCreate tylam inh results tympeg of
            Just t -> return t
            Nothing -> fail ("Illegal rule extension at : " ++ show r)

inferTypeExpr nt (MetaPeg mpeg) = undefined -- inferTypeMpeg nt mpeg 
inferTypeExpr nt (MetaExp mexp) = undefined -- inferTypeMExpr nt mexp


validateNt :: TyRuleEnv -> Type -> Type -> Maybe Type
validateNt nt inh rets =  

inferPegType :: NonTerminal -> APeg -> APegSt Type
inferPegType nt Lambda   = return TyAPeg
inferPegType nt (Lit _)  = return TyAPeg
inferPegType nt (NT nt' inh syn) = maybe (fail "Undefined non-terminal: " ++ nt)
                                         (\t -> do inhTys <- mapM inferTypeExpr nt'
                                                   synTys <- mapM inferTypeExpr nt' 
                                          )
