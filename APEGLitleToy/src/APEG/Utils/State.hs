{-|
Module      : APEG.Utils.State
Description : Representation of a state for the APEG interpreter/type-checker
Copyright   : (c) Leonardo Vieira dos Santos Reis, 2018
                  Rodrigo Geraldo Ribeiro, 2018
                  Elton M. Cardoso, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the definition of an APEG pure state used throughout the interpreter and type-checker.
The abstract data type allows for consulting and updating value, type environments and the input. 
-}

module APEG.Utils.State(
    PureState,
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
    withResult

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
newtype PureState a = PureState
                                (VEnv,      --  An envoironment of values  
                                 TyEnv,     --  A type enviroment for a rule and it's local variables. 
                                 TraceStr,  --  The current string accept by current rule. It may be a null String.
                                 Input,     --  The current input text. 
                                 Result a)  --  The Result of last operation. It may contain a value.

-- State for the APEG interpreter: 
type APegSt a = State (PureState a) 

-- | Builds a success Result from a particular value.
rVal :: a -> Result a 
rVal = Right

-- | Builds a failure Result from an error message.
rErr :: String -> Result a
rErr s = Left [s] 


-- | Add an error message to the result of the parser. 
-- If the result is a value, it is turned into an error and the value 
-- is lost.
stError ::  String -> PureState a ->  PureState a
stError s (ve,te,rec,inp,Left m) =  (ve,te,rec,inp,Left (s:m))
stError s (ve,te,rec,inp,_) =  (ve,te,rec,inp,Left [s])

 -- | Returns true whenever the result is not an error. 
stIsOk :: PureState a -> Bool
stIsOk (ve,te,rec,inp,Left _)  = False 
stIsOk (ve,te,rec,inp,Right _) = True

-- | 
valEnv :: PureState a -> VEnv
valEnv (ve,te,rec,inp,_) = ve

tyEnv :: PureState a -> TyEnv
tyEnv (ve,te,rec,inp,_) = te

upValEnv :: (VEnv -> VEnv) -> PureState a -> PureState a
upValEnv f (ve,te,rec,inp,r) = (f ve,te,rec,inp,r)

upTyEnv :: (TyEnv -> TyEnv) -> PureState a -> PureState a
upTyEnv f (ve,te,rec,inp,r) = (ve,f te,rec,inp,r)

withResult :: ([String] -> Result b) -> (a -> Result b) -> PureState a -> PureState b
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


zeroSt :: ApegGrm -> String -> PureState ()
zeroSt grm s = (M.fromList [("g",VLan grm)], M.fromList zs, nullTraceStr, s, Right ())
    where zs = map ruleEntry grm
          
