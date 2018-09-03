module APEGState where

import AbstractSyntax
import qualified Data.Map as M
import Control.Monad.State.Lazy

type VEnv  = M.Map String Value 
type TyEnv = M.Map String (Type , TyRuleEnv) 
type TyRuleEnv = M.Map String Type -- All the variables in the scope of a rule

type Input = String

type ApegTuple = (ApegGrm, -- The "Current" Grammar (Initially g0)
                  VEnv,  -- A stack of value enviroments   
                  TyEnv,   -- A Type Enviroment of rule and its variables. 
                  String,  -- the current string accept by the parser on the current rule.
                  Input,   -- The current input state. 
                  Bool)    -- The Result of las operation

type SmallTuple = (VEnv,  -- A stack of value enviroments   
                   String,  -- the current string accept by the parser on the current rule.
                   Input,   -- The current input state. 
                   Bool)    -- The Result of las operation

-- State for the APEG interpreter: 
type APegSt = State (ApegTuple) 




zeroSt :: ApegGrm -> String -> ApegTuple
zeroSt grm s = (grm,M.empty ,M.empty, [],s,True)

valIsMExpr :: Value -> Bool
valIsMExpr (VExp _) = True
valIsMExpr _        = False

valIsMPeg :: Value -> Bool
valIsMPeg (VPeg _) = True
valIsMPeg _        = False

strVal :: Value -> String
strVal (VStr s) = s

expFromVal :: Value -> Expr
expFromVal (VExp e) = e

varNameFromVal :: Value -> Var
varNameFromVal (VExp (Str s)) = s


apegFromVal :: Value -> APeg
apegFromVal (VPeg e) = e


matchAll :: [Type] -> [Type] -> Bool
matchAll xs ys = (and $ zipWith (==) xs ys) && ((length xs) == (length ys))


-- Projections anv Value Manipulation

language :: APegSt (ApegGrm)
language = get >>= \(grm,env,tyEnv,reckon,inp,res) -> return grm

var :: String -> APegSt Value
var s = get >>= (\(grm,env,tyEnv,reckon,inp,res) -> case env M.!? s of 
                                                      Nothing -> fail ("Undefined varibale " ++ s)
                                                      Just v   -> return v)

varSet :: String -> Value -> APegSt ()
varSet s v =  envAlter (M.insert s v)

varsSet :: [(String,Value)] -> APegSt ()
varsSet zs = envAlter (M.union (M.fromList zs))

envAlter :: (VEnv -> VEnv) -> APegSt ()
envAlter t = get >>= (\(grm,env,tyEnv,reckon,inp,res) -> put (grm,t env,tyEnv,reckon,inp,res)) 

tyEnvAlter :: (TyEnv -> TyEnv) -> APegSt ()
tyEnvAlter f = modify (\(grm,env,tyEnv,reckon,inp,res) -> (grm,env,f tyEnv,reckon,inp,res))

tyRuleAlter :: (TyRuleEnv -> TyRuleEnv) -> NonTerminal -> APegSt ()
tyRuleAlter f nt = modify (\(grm,env,tyEnv,reckon,inp,res) -> (grm,env,adapter tyEnv,reckon,inp,res))
    where adapter = M.adjust (\(t,renv) -> (t,f renv)) nt

ntType :: NonTerminal -> APegSt (Maybe Type)
ntType nt = do (_,_,tyEnv,_,_,_) <- get
               case  (tyEnv M.!? nt) of
                    Just (t,_) -> return $ Just t
                    Nothing    -> return $ Nothing
                    
recordNtType :: NonTerminal -> Type -> [(String,Type)] -> APegSt ()
recordNtType nt t xs = tyEnvAlter (M.insert nt (t,M.fromList xs))

varTypeOn :: NonTerminal -> Var -> APegSt (Maybe Type)
varTypeOn nt v = do (_,_,tyEnv,_,_,_) <- get
                    case  (tyEnv M.!? nt) of
                       Just (_,env) -> return $ env M.!? v 
                       Nothing    -> return $ Nothing

recordVarOn :: NonTerminal -> Var -> Type -> APegSt ()
recordVarOn nt v t = tyRuleAlter (M.insert v t) nt

recordVars :: NonTerminal -> [(Var, Type)] -> APegSt ()
recordVars nt xs = tyRuleAlter (M.union (M.fromList xs)) nt

envSwap :: VEnv ->  APegSt (VEnv) 
envSwap newEnv = do (g,env,tyEnv,reckon,inp,res) <- get
                    put(g,newEnv,tyEnv,reckon,inp,res) 
                    return env
                    
getStr :: APegSt (String) 
getStr = do (g,env,tyEnv,reckon,inp,res) <- get
            return reckon

resetStr :: APegSt () 
resetStr = modify (\(grm,env,tyEnv,reckon,inp,res) -> (grm,env,tyEnv,[],inp,res))
                     
isOk :: APegSt (Bool)
isOk = get >>= \(grm,env,tyEnv,reckon,inp,res) -> return res

supresEnv :: VEnv -> APegSt a -> APegSt a
supresEnv nwEnv b = do oldEnv <- envSwap nwEnv
                       r <- b
                       envSwap oldEnv
                       return r

envFromDec :: [(Type,Var)] -> [Value] -> VEnv
envFromDec xs vs = M.fromList $ zipWith (\(_,v) o -> (v,o)) xs vs

tyEnvFromDec :: [(Type,Var)] -> TyRuleEnv
tyEnvFromDec = (M.fromList).(map (\(a,b)->(b,a)))

