{-|
Module      : APEG.Interpreter.Value
Description : Value definition and utility functions.
Copyright   : 
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the definition of values as weel as predicates and utility functions for values manipulation.
-}

module APEG.Interpreter.Value(
    Value(..),
    valIsMExpr,
    valIsMPeg,
    expFromVal,
    typeFromVal,
    varNameFromVal,
    apegFromVal,
    vlan,
    vtype,
    vstr,
    strVal,
    intVal,
    vapeg,
    vexpr
    
          
) where

import APEG.AbstractSyntax
import qualified Data.Map as M
import Debug.Trace
import APEG.Interpreter.TypeEnvironment
import Debug.Trace

data Value = VStr String
           | VInt Int 
           | VFloar Float
           | VBool Bool
           | VMap (M.Map String Value)
           | VLan ApegGrm TyEnv
           | VGrm ApegGrm
           | VPeg APeg
           | VExp Expr
           | VType Type
           | Undefined
           deriving Show


-- | Construct an string from a value
vstr :: String -> Value
vstr = VStr

-- | Construct a language value from a Grammar.
vlan :: TyEnv -> ApegGrm -> Value
vlan ty gr = VLan gr ty 

-- | Construct a value from a Grammar.
vgrm :: ApegGrm -> Value
vgrm = VGrm

-- | construct a value from an APeg expression
vapeg :: APeg -> Value
vapeg = VPeg


-- | Construct a value from an expression
vexpr :: Expr -> Value
vexpr = VExp

-- | Construct a value from an type
vtype :: Type -> Value
vtype = VType



-- | Tests if a value is an expression.
valIsMExpr :: Value -> Bool
valIsMExpr (VExp _) = True
valIsMExpr _        = False

-- | Test if a value is a Peg.If the value does not contain an string, it will result in an error. 
valIsMPeg :: Value -> Bool
valIsMPeg (VPeg _) = True
valIsMPeg _        = False

-- | Return the String value of a value.
strVal :: Value -> String
strVal (VStr s) = s

intVal :: Value -> Int
intVal (VInt i) = i

-- | Return the Ggrammar value of a value.
grmFromVal :: Value -> ApegGrm
grmFromVal (VGrm s) = s

-- | Retrive the actual expression represented by  the value. If the value does not contain an expression, it will result in an error. 
expFromVal :: Value -> Expr
expFromVal (VExp e) = e
expFromVal x =  error ("expFromVal Unknow value: " ++ show x) 

-- | Retrive the actual type represented by the value. If the value does not contain a type, it will result in an error. 
typeFromVal :: Value -> Type
typeFromVal (VType t) = t

-- | Retrives the name of a variable. If the value does not contain an expression, whose is a variable, it will result in a error. 
varNameFromVal :: Value -> Var
varNameFromVal (VExp (Str s)) = s

-- | Retrives the APEG expression of a variable. If the value does not contain an APEG expression it will result in a error. 
apegFromVal :: Value -> APeg
apegFromVal (VPeg e) = e
