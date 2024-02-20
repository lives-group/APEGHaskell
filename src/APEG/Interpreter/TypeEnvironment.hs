{-|
Module      : APEG.Interpreter.State
Description : Representation of a state for the APEG interpreter/type-checker
Copyright   : 
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the definition of an APEG pure state (the actual datatype that repsents an state) used throughout the interpreter and type-checker.
The abstract data type allows for consulting and updating value, type environments and the input. 
-}

module APEG.Interpreter.TypeEnvironment where

import qualified Data.Map as M
import APEG.AbstractSyntax

-- | Type environment, a mapping from names to a non terminal 'Type' an 
-- it's inner enviroment
type TyEnv = M.Map String (Type , TyRuleEnv) 


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


pprintTyEnv :: TyEnv -> String
pprintTyEnv tye = unlines $ M.foldrWithKey (\k (tr,tre) l -> (k ++ " :: " ++ pprintType tr ++ " \n" ++ ppInnerEnv tre):l) [] tye
  where
      ppInnerEnv = unlines.ident.ppRintTyRuleEnv
      ident = map ("    "++)

ppRintTyRuleEnv :: TyRuleEnv -> [String]
ppRintTyRuleEnv m =  pprintMap pprintType m

pprintMap :: (a -> String) -> M.Map String a -> [String]
pprintMap ss m = M.foldrWithKey (\k e l -> (k ++ " :: " ++ ss e):l) [] m


