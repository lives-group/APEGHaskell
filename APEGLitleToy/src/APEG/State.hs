
{-|
Module      : APEG.State
Description : Representation of a state for the APEG interpreter/type-checker
Copyright   : (c) Leonardo Vieira dos Santos Reis, 2018
                  Rodrigo Geraldo Ribeiro, 2018
                  Elton M. Cardoso, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the definition of an APEG state used throughout the interpreter and type-checker.
The abstract data type allows for consulting and updating value and type environments, obtain the
string currently accepted by the recognizer, setting and consulting and transforming the results and
finally manipulation of the input.

-}

module APEG.State(
    APegTuple,
    Result,
    APegSt,
    VEnv,
    TyEnv,
    TyRuleEnv,
    TraceStr,
    Input,
    rVal,
    rErr,

    errorTuple,
    isOkTuple,
    valEnv,
    tyEnv,
    upValEnv,
    upTyEnv,
    withResult,
        
    valIsMExpr,
    valIsMPeg,
    strVal,
    expFromVal,
    varNameFromVal,
    apegFromVal,
        
    zeroSt,
    ruleEntry,
        
    isOk
    ) where
        

import APEG.AbstractSyntax
import qualified Data.Map as M
import Control.Monad.State.Lazy

-- | Value environment, a mapping form names to 'Values'
type VEnv  = M.Map String Value 

-- | Type environment, a mapping from names to a non terminal 'Type' an 
-- it's inner enviroment
type TyEnv = M.Map String (Type , TyRuleEnv) 

-- | Type environment for a rule. Maps varialble names to 
type TyRuleEnv = M.Map String Type 

-- | The recognizer input is represented by an String.
type Input = String 

-- | The result buit by a parser. Can either be an list of error messages or
-- an actual value of some type a.
type Result a = Either [String] a
    


-- | APeggTuple is the state representation for the APEG interpreter. It consists of
-- an value enviroment 'VEnv', which contains the mapping of names to values, a type
-- enviroment 'TyEnv', that contains all related information about the types of rules
-- an its attributes. The 'TraceStr' argument keeps track of the string being recognized
-- since the last APEG Bind Command. The 'Input' contais the input to be passed to the parser.
-- The 'Result' is the result built by the parser.
type APegTuple a =
                   (VEnv,      --  An envoironment of values  
                    TyEnv,     --  A type enviroment for a rule and it's local variables. 
                    TraceStr,  --  The current string accept by current rule. It may be a null String.
                    Input,     --  The current input text. 
                    Result a)  --  The Result of last operation. It may contain a value.

type SmallTuple = (VEnv,    --  A stack of value enviroments   
                   String,  --  the current string accept by the parser on the current rule.
                   Input,   --  The current input state. 
                   Bool)    --  The Result of las operation

-- State for the APEG interpreter: 
type APegSt a = State (APegTuple a) 

-- | Builds a success Result from a particular value.
rVal :: a -> Result a 
rVal = Right

-- | Builds a failure Result from an error message.
rErr :: String -> Result a
rErr s = Left [s] 


-- | Adds an error message to the result of t
error ::  String -> APegTuple a ->  APegTuple a
errorTuple s (ve,te,rec,inp,Left m) =  (ve,te,rec,inp,Left (s:m))
errorTuple s (ve,te,rec,inp,_) =  (ve,te,rec,inp,Left [s])

isOkTuple :: APegTuple a -> Bool
isOkTuple (ve,te,rec,inp,Left _)  = False 
isOkTuple (ve,te,rec,inp,Right _) = True

valEnv :: APegTuple a -> VEnv
valEnv (ve,te,rec,inp,_) = ve

tyEnv :: APegTuple a -> TyEnv
tyEnv (ve,te,rec,inp,_) = te

upValEnv :: (VEnv -> VEnv) -> APegTuple a -> APegTuple a
upValEnv f (ve,te,rec,inp,r) = (f ve,te,rec,inp,r)

upTyEnv :: (TyEnv -> TyEnv) -> APegTuple a -> APegTuple a
upTyEnv f (ve,te,rec,inp,r) = (ve,f te,rec,inp,r)

withResult :: ([String] -> Result b) -> (a -> Result b) -> APegTuple a -> APegTuple b
withResult err f (ve,te,rec,inp,Left s) = (ve,te,rec,inp, err s)
withResult err f (ve,te,rec,inp,Right r) = (ve,te,rec,inp, f r)


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

zeroSt :: ApegGrm -> String -> APegTuple ()
zeroSt grm s = (M.fromList [("g",VLan grm)], M.fromList zs, nullTraceStr, s, Right ())
    where zs = map ruleEntry grm


ruleEntry :: ApegRule -> (NonTerminal,(Type, TyRuleEnv))
ruleEntry (ApegRule nt inh syn _ ) = (nt,(TyRule (map fst inh) (map fst syn),M.fromList (map (\(a,b) -> (b,a)) inh)))    


{-

isOk :: APegSt () Bool
isOk = get >>= return.isOkTuple

matchAll :: [Type] -> [Type] -> Bool
matchAll xs ys = (and $ zipWith (==) xs ys) && ((length xs) == (length ys))

tyEnvFromRule :: ApegRule -> TyEnv
tyEnvFromRule r = M.fromList [ruleEntry r]

-- Projections anv Value Manipulation

language :: APegSt (ApegGrm)
language = get >>= \(grm,env,tyEnv,reckon,inp,res) -> return grm

langExt :: ApegGrm -> APegSt ()
langExt grm 
     = do g  <- language
          g' <- joinRules g grm
          modify (\(grm,env,tyEnv,reckon,inp,res) -> (g',env,tyEnv,reckon,inp, res))
          
joinRules ::  ApegGrm -> ApegGrm -> APegSt ApegGrm
joinRules g []= return g
joinRules g (x:xs) = grmAddRule g x >>= \r -> joinRules r xs

var :: String -> APegSt Value
var s = get >>= (\(grm,env,tyEnv,reckon,inp,res) -> case env M.!? s of 
                                                      Nothing -> fail ("Undefined varibale " ++ s ++
                                                                       " remaining input = " ++ inp)
                                                      Just v   -> return v)

varSet :: String -> Value -> APegSt ()
varSet s v =  envAlter (M.insert s v)

varsSet :: [(String,Value)] -> APegSt ()
varsSet zs = envAlter (M.union (M.fromList zs))


grmAddRule :: ApegGrm -> ApegRule -> APegSt ApegGrm
grmAddRule [] r = return $ [r]
grmAddRule (x@(ApegRule nt inh syn body):xs) r@(ApegRule nt' inh' syn' body')
    | (nt == nt') && (inh == inh') && (syn == syn') = return $ ApegRule nt inh syn (mkAltBody body body'):xs
    | (nt == nt') && ((inh /= inh') || (syn /= syn')) = fail ("Conflicting definitions of " ++ nt ++ " when composing languages.")
    | (nt /= nt') = grmAddRule xs r >>= return.(x:)

envAlter :: (VEnv -> VEnv) -> APegSt ()
envAlter t = get >>= (\(grm,env,tyEnv,reckon,inp,res) -> put (grm,t env,tyEnv,reckon,inp,res)) 

tyEnv :: APegSt (TyEnv)
tyEnv =  get >>= (\(grm,env,tyEnv,reckon,inp,res) -> return tyEnv)

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

ruleEnv :: NonTerminal -> APegSt (Maybe (Type,TyRuleEnv))
ruleEnv nt = do (_,_,tyEnv,_,_,_) <- get
                case  (tyEnv M.!? nt) of
                   Just t -> return $ Just t
                   Nothing    -> return $ Nothing

recordNtType :: NonTerminal -> [(String,Type)] -> [Type] -> APegSt ()
recordNtType nt xs ys = tyEnvAlter (M.insert nt (TyRule (map snd xs) ys, M.fromList xs))


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
                    
tyEnvSwap :: TyEnv ->  APegSt (TyEnv) 
tyEnvSwap newEnv = do (g,env,tyEnv,reckon,inp,res) <- get
                      put(g,env,newEnv,reckon,inp,res) 
                      return tyEnv
                                        
                    
getStr :: APegSt (String) 
getStr = do (g,env,tyEnv,reckon,inp,res) <- get
            return reckon

resetStr :: APegSt () 
resetStr = modify (\(grm,env,tyEnv,reckon,inp,res) -> (grm,env,tyEnv,[],inp,res))
                     


supresEnv :: VEnv -> APegSt a -> APegSt a
supresEnv nwEnv b = do oldEnv <- envSwap nwEnv
                       r <- b
                       envSwap oldEnv
                       return r

envFromDec :: [(Type,Var)] -> [Value] -> VEnv
envFromDec xs vs = M.fromList $ zipWith (\(_,v) o -> (v,o)) xs vs

tyEnvFromDec :: [(Type,Var)] -> TyRuleEnv
tyEnvFromDec = (M.fromList).(map (\(a,b)->(b,a)))


supresTyEnv :: TyEnv -> APegSt a -> APegSt a
supresTyEnv nwEnv b = do oldEnv <- tyEnvSwap nwEnv
                         r <- b
                         tyEnvSwap oldEnv
                         return r-}
