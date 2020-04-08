{-|
Module      : APEG.Interpreter.DT
Description : Representation of a simple derivation tree.
Copyright   : (c) Leonardo Vieira dos Santos Reis, 2020
                  Rodrigo Geraldo Ribeiro, 2020
                  Elton M. Cardoso, 2020
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains a definition of a very simple derivation tree (not an actual AST) that is constructed whenever the interpreter runs over an input.
-}

module APEG.Interpreter.DT(
   DT,
   dtRoot,
   dtLeaf,
   dtNull,
) where

import APEG.AbstractSyntax

-- | DT representes a Derivation Tree that also carries aditional infromartion of type a.
data DT a = DTN String a  [DT a] -- | Non Terminal Node Constructor: Recieves the name of the rule (Non Terminal), a info of type a and a list of Childs.
        | DTT a String           -- | Terminal Node Constructor: the info of type a and the literal value ot the terminal.
        | DTNIL                  -- | A null node
        deriving (Eq)
        

dtRoot :: String -> a  -> [DT a] -> DT a
dtRoot = DTN

dtLeaf :: a -> String -> DT a
dtLeaf = DTT

dtNull :: DT a
dtNull = DTNIL

instance (Show a) => Show (DT a)  where
    show (DTN s v xs) = "[" ++ s ++ (concatMap show xs)  ++"]" 
    show (DTT v s) = "<" ++ s ++ ">"
    show DTNIL = "<NIL>"
