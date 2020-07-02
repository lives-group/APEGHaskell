{-|
Module      : APEG.Interpreter.State
Description : Representation of a state for the APEG interpreter/type-checker
Copyright   : (c) Leonardo Vieira dos Santos Reis, 2018
                  Rodrigo Geraldo Ribeiro, 2018
                  Elton M. Cardoso, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the definition of an APEG pure state (the actual datatype that repsents an state) used throughout the interpreter and type-checker.
The abstract data type allows for consulting and updating value, type environments and the input. 
-}

module APEG.Interpreter.State(
    PureState,
    Result,
    VEnv,
    DTV,
    TyEnv,
    TyRuleEnv,
    Input,
    
    rVal,
    rErr,
    assureOkStatus,
    remInp,
    remInpM,
    setResult,
    accPrefix,
    getPrefix,
    resetPrefix,
    setPrefix, 
    match,
    prependPrefix,
    setInput,
    language,
    getResult,
    stIsOk,
    stErrorMsg,
    valEnv,
    tyEnv,
    upValEnv,
    upTyEnv,
    withResult,
    tyEnvFromRule,
    zeroSt,
    pprintTyEnv,    
    ppRintTyRuleEnv,
    quickShow
) where

import APEG.AbstractSyntax
import qualified Data.Map as M
import APEG.Interpreter.Value
import APEG.Interpreter.MaybeString
import APEG.Interpreter.DT


-- | Value environment, a mapping form names to 'Values'
type VEnv  = M.Map String Value 

-- | A derivation tree whose node also carries the local enviroment of the rule (usefull for debugging !)
type DTV = DT VEnv

-- | Type environment, a mapping from names to a non terminal 'Type' an 
-- it's inner enviroment
type TyEnv = M.Map String (Type , TyRuleEnv) 
--
-- Thye type enviroment layout is like this:
--
-- r<int x returns  int y> :=  { x = 10} 'a' r<x+1,y> / 'b' {y = 1;}
--  -----------------------------------------
--  r   |  Type (of the rule) | TyRuleEnv (Rule local declaration types)
--      |                     | ---------------- 
--      |                     |  x : int
--      |  int -> int         |  y : int
--      |                     |  z : int
--      |                     | ----------------
--  -----------------------------------------
--

-- | Type environment for a rule. Maps varialble names to 
type TyRuleEnv = M.Map String Type 

-- | The recognizer input is represented by an String.
type Input = String 

-- | The result buit by a parser. Can either be an list of error messages or
-- an actual value of some type a.
type Result = Either [String] [DTV]
    


-- | APeggTuple is the state representation for the APEG interpreter. It consists of
-- an value enviroment 'VEnv', which contains the mapping of names to values, a type
-- enviroment 'TyEnv', that contains all related information about the types of rules
-- an its attributes. The 'MybStr' argument keeps track of the string being recognized
-- since the last APEG Bind Command. The 'Input' contais the input to be passed to the parser.
-- The 'Result' is the result built by the parser.
newtype PureState =   PureState (VEnv,     --  An envoironment of values  
                                 TyEnv,    --  A type enviroment for a rule and it's local variables. 
                                 MybStr,   --  The current string accept by current rule. It may be a null String.
                                 Input,    --  The current input text. 
                                 Result)   --  The Result of last operation. It may contain a list of derivation tree nodes.
                                 deriving Show


-- | Returns the current language attribute.
language :: PureState -> ApegGrm
language (PureState (ve,te,rec,inp,res)) = case ve M.!?  "g" of
                                                Just (VLan grm) -> grm
                                                Just ( _ )     -> error "Unexpected value for language attribute 'g' "
                                                Nothing        -> error "Undefined value for language attribute 'g'"


getResult :: PureState -> Result
getResult (PureState (_,_,_,_,r)) = r

-- | Builds a success Result from a particular node value.
rVal :: DTV -> Result
rVal = Right.(:[])



-- | Builds a failure Result from an error message.
rErr :: String -> Result
rErr s = Left [s] 

 -- | Make sure that state is OK. If it isn't then make it ok with a null list of nodes
assureOkStatus :: PureState -> PureState
assureOkStatus (PureState (e,t,p,i,Left _) ) = PureState (e,t,p,i,Right [])
assureOkStatus s = s

-- | Set the result of the state. 
setResult :: Result -> PureState -> PureState
setResult r' (PureState (e,t,p,i,_))  = PureState (e,t,p,i,r')


-- | Returns the MybStr of the accepterd prefix.
getPrefix :: PureState -> MybStr
getPrefix (PureState (_,_,mbs,_,_)) = mbs

-- | Returns the consumed prefix of the input, if any !
accPrefix :: PureState -> Maybe String
accPrefix (PureState (_,_,mbs,_,_)) = mbStr mbs


-- | Concat a given string to the beginning of the current accepted string. 
prependPrefix :: MybStr -> PureState -> PureState
prependPrefix s (PureState (e,t,p,i,r)) = PureState (e,t,s ?++ p,i,r)

-- | Resets the cosumed prefix of the input to empty string
resetPrefix :: PureState -> PureState
resetPrefix (PureState (e,t,_,inp,r)) = PureState (e,t,emptyMybStr,inp,r)

-- | Returns the remaining input of the state.
remInp :: PureState -> String
remInp (PureState (_,_,_,inp,_)) = inp 


-- | Returns the remaining input of the state, with the maximum of n characters.
remInpM :: Int -> PureState -> String
remInpM n (PureState (_,_,_,inp,_)) = roundInput n inp 

splitPrefix :: String -> String -> (String,String)
splitPrefix xs ys = test (splitAt (length xs) ys)
   where
       test (p,zs)
           | p == xs = (p,zs)
           | otherwise = ([],ys)

           
-- | Matches a string 's' against the current input. Either 's' mathes the input and the input is consumed or
-- 's' dont matches the input and no input is consumed.
match :: String -> PureState -> PureState
match s (PureState (e,t,p,i,r)) = case splitPrefix s i of
                                     ([],_)  -> PureState (e,t,p,i,Left ["Could no match " ++ s ++ " against " ++ (roundInput 20 i)])
                                     (xs,ys) -> PureState (e,t,p ++> xs,ys, fmap (dtLeaf e xs:) r)

roundInput :: Int -> String -> String
roundInput n s 
    | length s <= n = '\"':s ++ "\""
    | otherwise = '\"':(take n s) ++ "...\""


setPrefix ::MybStr ->  PureState ->  PureState
setPrefix p' (PureState (e,t,p,i,r)) = PureState (e,t,p',i,r)

setInput :: String -> PureState -> PureState
setInput i'(PureState (e,t,p,i,r)) = PureState (e,t,p,i',r)


-- | Add an error message to the result of the parser. 
-- If the result is a value, it is turned into an error and the value 
-- is lost.
stError ::  String -> PureState ->  PureState
stError s (PureState (ve,te,rec,inp,Left m)) =  PureState (ve,te,rec,inp,Left (s:m))
stError s (PureState (ve,te,rec,inp,_)) =  PureState (ve,te,rec,inp,Left [s])

 -- | Returns true whenever the result is not an error. 
stIsOk :: PureState -> Bool
stIsOk (PureState (ve,te,rec,inp,Left _))  = False 
stIsOk (PureState (ve,te,rec,inp,Right _)) = True


-- | Retrive Error messages from the state. If the state isn't an error
-- | state then this function returns an empty list.
stErrorMsg :: PureState -> [String]
stErrorMsg (PureState (_,_,_,_,Left xs)) = xs
stErrorMsg (PureState (_,_,_,_,Right _)) = []

-- | Returns the value environment
valEnv :: PureState -> VEnv
valEnv (PureState (ve,te,rec,inp,_)) = ve

-- | Returns the tyep environment 
tyEnv :: PureState -> TyEnv
tyEnv (PureState (ve,te,rec,inp,_)) = te

-- | Update value environment with the supplied function. 
upValEnv :: (VEnv -> VEnv) -> PureState -> PureState
upValEnv f (PureState (ve,te,rec,inp,r)) = PureState (f ve,te,rec,inp,r)

-- | Update type environment with the supplied function. 
upTyEnv :: (TyEnv -> TyEnv) -> PureState -> PureState
upTyEnv f (PureState (ve,te,rec,inp,r)) = (PureState (ve,f te,rec,inp,r))


-- | Uses the type result in a computation. Two functions must be supplied, the first one
-- is used in case the result is an error, the scond one is used in a result that has a value.
withResult :: ([String] -> Result) -> ([DTV] -> Result) -> PureState -> PureState
withResult err f (PureState (ve,te,rec,inp,Left s)) = PureState (ve,te,rec,inp, err s)
withResult err f (PureState (ve,te,rec,inp,Right r)) = PureState (ve,te,rec,inp, f r)


-- | RConstruct an Initial state form a given Grammar and a input string.
zeroSt :: ApegGrm -> String -> PureState
zeroSt grm s = PureState (M.fromList [("g",VLan grm)], M.fromList zs, nullMybStr, s, Right [])
    where zs = map ruleEntry grm
          
-- | Constructs a type environment from a rule definition. 
-- | The tuple produced is suitable for insertion in a TyEnv and is formed by 
-- | the non terminal name, its type and a litle environment conaining its parameters.
ruleEntry :: ApegRule -> (NonTerminal,(Type, TyRuleEnv))
ruleEntry (ApegRule nt inh syn _ ) = (nt,(TyRule (map fst inh) (map fst syn),M.fromList (map (\(a,b) -> (b,a)) inh))) 

-- | Constructs a tyEnv from a Rule definition.
tyEnvFromRule :: ApegRule -> TyEnv
tyEnvFromRule r = M.fromList [ruleEntry r]

pprintTyEnv :: TyEnv -> String
pprintTyEnv tye = unlines $ M.foldrWithKey (\k (tr,tre) l -> (k ++ " :: " ++ pprintType tr ++ " \n" ++ ppInnerEnv tre):l) [] tye
  where
      ppInnerEnv = unlines.ident.ppRintTyRuleEnv
      ident = map ("    "++)

ppRintTyRuleEnv :: TyRuleEnv -> [String]
ppRintTyRuleEnv m =  pprintMap pprintType m

pprintMap :: (a -> String) -> M.Map String a -> [String]
pprintMap ss m = M.foldrWithKey (\k e l -> (k ++ " :: " ++ ss e):l) [] m



-- | Simplified version of show that only prints the input and recognized input aspects of the state.          
quickShow :: PureState -> String
quickShow (PureState (_,_,r,i,_) ) = "(" ++ fromMybStr r++ "," ++ i++")"
