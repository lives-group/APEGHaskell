
{-|
Module      : APEG.Interpreter.MonadicState
Description : Representation of a state for the APEG interpreter/type-checker
Copyright   : 
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the definition of an APEG state used throughout the interpreter and type-checker.
The abstract data type allows for consulting and updating value and type environments, obtain the
string currently accepted by the recognizer, setting and consulting and transforming the results and
finally manipulation of the input.

-}

module APEG.Interpreter.MonadicState where
        

import APEG.AbstractSyntax
import qualified Data.Map as M
import Control.Monad.State.Lazy
import APEG.Interpreter.MaybeString
import APEG.Interpreter.State
import APEG.Interpreter.Value
import APEG.Interpreter.DT


-- | State for the APEG interpreter: 
type APegSt = State PureState 

-- | Is state Ok ? 
isOk :: APegSt Bool
isOk = get >>= return.stIsOk

-- | Change the result to Ok. A empty tree node list is inserted as the result.
done :: APegSt ()
done =  modify assureOkStatus 

-- | Change the result to Ok and the result with a give Derivation Tree as result.
doneWith :: DTV -> APegSt ()
doneWith t = modify (setResult (rVal t)) 

-- | Change the result to Fail, whith no errors. 
pfail :: APegSt ()
pfail = modify (setResult (rErr []))

-- | Change the result to Fail, whith the give error message. 
pfailMsg :: String -> APegSt ()
pfailMsg s = modify (setResult (rErr s))


swapResult :: Result ->  APegSt (Result)
swapResult r = do r' <- get >>= return.getResult
                  modify (\rs-> setResult r rs)
                  return r'

dtRuleBuild :: APegSt () -> APegSt ()
dtRuleBuild c = do r <- swapResult (Right [])
                   c
                   modify (\rs-> setResult (catResult (getResult rs) r) rs)
                   

patternMatch :: String -> APegSt ()
patternMatch s = modify (match s)


-- | Return the laguage attribute.
getLanguage :: APegSt (ApegGrm, TyEnv)
getLanguage = get >>= return.language

     
-- | Access the value of a variabel. If variable's name is undefined, the result will be an runtime error.
var :: String -> APegSt Value
var s = get >>= (\pst -> case (valEnv pst) M.!? s of 
                          Nothing -> error ("Undefined varibale " ++ s ++" remaining input = " ++ (remInp pst))
                          Just v   -> return v)

-- | Updates the value environment with a give update function.                           
envAlter :: (VEnv -> VEnv) -> APegSt ()
envAlter t = modify  (upValEnv t) 

-- | Set the value of a specified variable in the evironment. If the variable does not exists, 
-- it will be created. It it already exists it's value will be updated.  
varSet :: String -> Value -> APegSt ()
varSet s v =  envAlter (M.insert s v)

-- | List version of varSet.
varsSet :: [(String,Value)] -> APegSt ()
varsSet zs = envAlter (M.union (M.fromList zs))

-- | Returns the type environment (type table).
getTyEnv :: APegSt (TyEnv)
getTyEnv =  get >>= (return.tyEnv)

-- | Update the type environment wiht the given function.
tyEnvAlter :: (TyEnv -> TyEnv) -> APegSt ()
tyEnvAlter f = modify (upTyEnv f)

-- | Update an entry of a type environment (the local environment of a particular rule) with the given function.
tyRuleAlter :: (TyRuleEnv -> TyRuleEnv) -> NonTerminal -> APegSt ()
tyRuleAlter f nt = modify (upTyEnv adapter)
    where adapter = M.adjust (\(t,renv) -> (t,f renv)) nt


-- | Recover the type of a non terminal (nt) from the type environment. Returns Just t if nt
-- has type t on the environment or Nothing if nt is not in the environment.
ntType :: NonTerminal -> APegSt (Maybe Type)
ntType nt = do tyEnv <- getTyEnv
               case  (tyEnv M.!? nt) of
                    Just (t,_) -> return $ Just t
                    Nothing    -> return $ Nothing
                   

-- | Returns the local type environment of aspecific rule.
-- Returns Nothing if there is no matching rule name or the rule has'nt a type environment.
ruleEnv :: NonTerminal -> APegSt (Maybe (Type,TyRuleEnv))
ruleEnv nt = do tyEnv <- getTyEnv
                case  (tyEnv M.!? nt) of
                   Just t -> return $ Just t
                   Nothing    -> return $ Nothing
                   
                   
-- | Returns the local type table of aspecific rule, withou the rule type.
-- Returns a empty envuronment if there is no matching rule name or the rule hasn't a type environment.
localRuleEnv :: NonTerminal -> APegSt (TyRuleEnv)
localRuleEnv nt = do tyEnv <- getTyEnv
                     case  (tyEnv M.!? nt) of
                       Just (_,t)   -> return  t
                       Nothing  -> return M.empty

-- | Record a type for a non terminal in the type environment.
recordNtType :: NonTerminal -> [(String,Type)] -> [Type] -> APegSt ()
recordNtType nt xs ys = tyEnvAlter (M.insert nt (TyRule (map snd xs) ys, M.fromList xs))

-- | Return the type of a varible in the conetexct of a specfic rule. 
varTypeOn :: NonTerminal -> Var -> APegSt (Maybe Type)
varTypeOn nt v = do tyEnv <- getTyEnv
                    case  (tyEnv M.!? nt) of
                       Just (_,env) -> return $ env M.!? v 
                       Nothing    -> return $ Nothing

-- |  Register the type of a variable in the type environment.
-- If the variable had a previous entry it will be updated
recordVarOn :: NonTerminal -> Var -> Type -> APegSt ()
recordVarOn nt v t = tyRuleAlter (M.insert v t) nt

-- | List version of recordVarOn
recordVars :: NonTerminal -> [(Var, Type)] -> APegSt ()
recordVars nt xs = tyRuleAlter (M.union (M.fromList xs)) nt

-- |  Swaps the value environment for a given one. Returns the old envrionment.
envSwap :: VEnv ->  APegSt (VEnv) 
envSwap newEnv = do pst <- get
                    put $ upValEnv (\env -> newEnv) pst
                    return $ valEnv pst

-- | Swaps the type environment for a given one. Returns the old envrionment.
tyEnvSwap :: TyEnv ->  APegSt (TyEnv) 
tyEnvSwap newEnv = do pst <- get
                      put $ upTyEnv (\env -> newEnv) pst
                      return $ tyEnv pst
                                        
-- | Swaps the type local rule environment (if it exists) for a given one. Returns the old envrionment.
-- If the local rule environment does not exists, an new empty rule enviroment is returned.
tyRuleEnvSwap :: NonTerminal -> TyRuleEnv ->  APegSt (TyRuleEnv) 
tyRuleEnvSwap nt newEnv = do r <- ruleEnv nt
                             case r of
                                  Just (nty, tyenv ) -> tyEnvAlter (M.insert nt (nty,newEnv)) >> return tyenv
                                  Nothing            -> return M.empty
                                        

-- | Returns the consumed input prefix util this point, since the last time
-- it began to be recorded.
getStr :: APegSt (MybStr) 
getStr =  get >>= return.getPrefix


-- | Resets the accumator of accepted input to the empty string.  
resetStr :: APegSt () 
resetStr = modify resetPrefix
                     
              
-- | Overlaps the current environment with the give one and procees the given action restoing the the old environment.
-- overlpaEnv e a executes the action 'a'in the given environment 'e'. After the execuction of 'a' the environment is restored
-- to what it was before.
overlapEnv :: VEnv -> APegSt a -> APegSt a
overlapEnv nwEnv b = do oldEnv <- envSwap nwEnv
                        r <- b
                        envSwap oldEnv
                        return r
                       
-- | Behaves like overlapEnv except its targed is the type environment.
overlapTyEnv :: TyEnv -> APegSt a -> APegSt a
overlapTyEnv nwEnv b = do oldEnv <- tyEnvSwap nwEnv
                          r <- b
                          tyEnvSwap oldEnv
                          return r

                       
-- | Constructs a value enviroment from a set of declaration variables and a set of initial values.
envFromDec :: [(Type,Var)] -> [Value] -> VEnv
envFromDec xs vs = M.fromList $ zipWith (\(_,v) o -> (v,o)) xs vs

-- | Constructs a type environment from a set of variable declarations.
tyEnvFromDec :: [(Type,Var)] -> TyRuleEnv
tyEnvFromDec = (M.fromList).(map (\(a,b)->(b,a)))


