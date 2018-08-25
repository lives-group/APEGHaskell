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
                
type APegSt = State (ApegGrm, VEnv, Input)

data Value = VStr String
           | VMap VEnv
           | VLan [ApegRule]
           | VMeta MAPeg
           | Undefined
           deriving Show

matchAll :: [Type] -> [Type] -> Bool
matchAll xs ys = (and $ zipWith (==) xs ys) && ((length xs) == (length ys))


-- inferType :: TyRuleEnv -> MExpr -> Type
-- inferType re (MkStr _)    = TyStr
-- inferType re (MkEmptyMap) = TyMap TyStr
-- inferType re (MkVar v)    = maybe (re !? v) ()


unMeta :: MAPeg -> APeg
unMeta MkLambda           = Lambda
unMeta (MkCal nt inh syn) = NT nt (map unMetaE inh) (map unMetaE syn)
unMeta (MkKle e)          = Kle (unMeta e)
unMeta (MkNot e)          = Not (unMeta e)
unMeta (MkSeq xs)         = Seq (map unMeta xs)
unMeta (MkAlt xs)         = Alt (map unMeta xs)
unMeta (MkAE xs)          = AEAttr $ map (\(v,e) -> (v,unMetaE e)) xs

unMetaE :: MExpr -> Expr
unMetaE (MkStr s)  = Str s
unMetaE (MkEmptyMap) = EmptyMap
unMetaE (MkVar v) = EVar v
unMetaE (MkMp xs) = Mp (map (\(v,e) -> (v,unMetaE e)) xs)
unMetaE (MkMapIns eb s ee) = MapIns (unMetaE eb) s (unMetaE ee)
unMetaE (MkMapAcces me ma ) = MapAcces (unMetaE me) (unMetaE ma) 


var :: String -> APegSt Value
var s = get >>= (\(grm,env,inp) -> case env M.!? s of 
                                        Nothing -> fail "Undefined varibale"
                                        Just v   -> return v)



-- =================== Monad Utilityes =================== --

evalExp :: Expr -> APegSt (Value)
evalExp (Str s) = return (VStr s)
evalExp (EmptyMap) = return (VMap M.empty)
evalExp (EVar v)   = var v

