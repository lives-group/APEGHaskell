
{-|
Module      : APEG.APEGExprParser
Description : Parser combinators and parser utility for APEG Attribute Expression language.
Copyright   : (c) Leonardo Vieira dos Santos Reis, 2020
                  Rodrigo Geraldo Ribeiro, 2020
                  Elton M. Cardoso, 2020
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the parser implementation for a concrete syntax
of the APEG attribute expression language.
-}

module APEG.Parser.APEGExprParser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Char
--import Text.ParserCombinators.Parsec.Char
import APEG.AbstractSyntax
import Data.Functor.Identity


ident :: Parser String
ident = (lower >>= (\x -> many alphaNum >>= (\xs -> return (x:xs)))) <?> "Attribute indentifier"

lambda :: Parser APeg
lambda = (string "Lmb" >> return Lambda) <?> "lambda"

literal :: Parser APeg
literal = do string "'"
             xs <- many (noneOf "'\n")
             string "'"
             return (Lit xs)

bind :: Parser APeg 
bind = do i <- ident 
          spaces
          string "<-"
          spaces
          e <- literal
          return (Bind i e)
          
identSym :: Parser APeg
identSym = do i <- ident 
              identContinue i

identContinue :: String -> Parser APeg
identContinue i = (paramList >>= \(h,s) -> return (NT i h s) ) <|>
                  try (do spaces 
                          string "<-" 
                          spaces 
                          e <- apegExp 
                          return (Bind i e) )
                  
paramList :: Parser ([Expr],[Var])
paramList = string "<|>" >> return ([],[])

parens :: Parser a -> Parser a
parens p = do char '('
              r <- p
              char ')'
              return r

apegTerm :: Parser APeg
apegTerm = spaces >> (literal <|> lambda <|> identSym <|> (parens apegExp))
           
apegExp :: Parser APeg          
apegExp = buildExpressionParser table (apegTerm >>= \t -> spaces >> return t) 
         <?> "expression"

table :: OperatorTable String () Identity APeg
table   = [ [prefix "!" Not, postfix "*" Kle ],
            [seqOp],
            [binary "|" (Alt) AssocLeft ]
           ]
           
binary :: String -> (APeg -> APeg -> APeg) -> Assoc -> Operator String () Identity APeg  
binary  name fun assoc = Infix   (do{ string name; spaces ;return fun }) assoc

prefix :: String -> (APeg -> APeg) -> Operator String () Identity APeg  
prefix  name fun       = Prefix  (do{ string name; spaces; return fun })

postfix :: String -> (APeg -> APeg) -> Operator String () Identity APeg  
postfix name fun       = Postfix (do{ spaces;string name; spaces; return fun }) 

seqOp :: Operator String () Identity APeg  
seqOp = Infix   (try (lookAhead apegTerm >> return Seq)) AssocLeft
