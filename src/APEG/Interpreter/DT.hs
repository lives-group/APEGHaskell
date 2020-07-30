{-|
Module      : APEG.Interpreter.DT
Description : Representation of a simple derivation tree.
Copyright   : 
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
   pprintDT,
) where

import APEG.AbstractSyntax
import Data.List

-- | DT representes a Derivation Tree that also carries aditional infromartion of type a.
data DT a = DTN String a  [DT a] -- | Non Terminal Node Constructor: Recieves the name of the rule (Non Terminal), a info of type a and a list of Childs.
        | DTT a String           -- | Terminal Node Constructor: the info of type a and the literal value ot the terminal.
        | DTNIL                  -- | A null node
        deriving (Eq)
        

pprintDT :: DT a -> String
pprintDT (DTT _ s)     = s
pprintDT (DTNIL)       = ""
pprintDT (DTN s _ xs ) = s ++ "\n" ++ (unlines (textTree ""  xs))

nodeName :: DT a -> String
nodeName DTNIL       = ""
nodeName (DTT _ s)   = s
nodeName (DTN s _ _) = s
 
 
textTree :: String -> [DT a] -> [String]
textTree prfx []     = [] 
textTree prfx  [n]   = [prfx ++ "\x2514\x2500 " ++ (nodeName n)]
textTree prfx ((DTT _ s):xs) = (prfx ++ "\x251c\x2500 " ++ (warpChar s))  :  (textTree prfx xs)
textTree prfx ((DTN s _ ys):xs) = (prfx ++ "\x251c\x2500 " ++ (warpChar s))  : (textTree (prfx ++ "\x2502  ") ys) ++ (textTree prfx xs)



dtRoot :: String -> a  -> [DT a] -> DT a
dtRoot = DTN

dtLeaf :: a -> String -> DT a
dtLeaf = DTT

dtNull :: DT a
dtNull = DTNIL

warpChar :: String -> String 
warpChar [] = []
warpChar ('\n':xs) = '\\':'n': warpChar xs
warpChar ('\t':xs) = '\\':'t': warpChar xs
warpChar ('\r':xs) =  '\\':'r': warpChar xs
warpChar (x:xs) =  x: warpChar xs

instance (Show a) => Show (DT a)  where
    show (DTN s v xs) = "DTN \"" ++ s ++ "\" [" ++ (concat $ intersperse "," (map show xs))  ++"]" 
    show (DTT v s) = "DTT \"" ++ (warpChar s) ++ "\""
    show DTNIL = "<NIL>"
