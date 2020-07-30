
{-|
Module      : APEG.APEGParser
Description : Parser combinators and parser utility for APEG.
Copyright   : (c) Leonardo Vieira dos Santos Reis, 2020
                  Rodrigo Geraldo Ribeiro, 2020
                  Elton M. Cardoso, 2020
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module contains the parser implementation for a concrete syntax
of the APEG litle toy. This is a temporary syntax for easy the pain
of writing test programs and can be changed at any time.

-}

module APEG.Parser.APEGParser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Char
--import Text.ParserCombinators.Parsec.Char
import APEG.AbstractSyntax
--import APEG.Parser.APEGExprParser
import Data.Functor.Identity



ident :: Parser String
ident = (lower >>= (\x -> many alphaNum >>= (\xs -> return (x:xs)))) <?> "Attribute indentifier"

lambda :: Parser APeg
lambda = (string "Lam" >> return Lambda) <?> "lambda"

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
apegExp = buildExpressionParser apegTable (apegTerm >>= \t -> spaces >> return t) 
         <?> "APEG Expression"

apegTable :: OperatorTable String () Identity APeg
apegTable   = [ [prefix "!" Not, postfix "*" Kle ],
                [seqOp],
                [binary "|" (Alt) AssocLeft ]
              ]
           
binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a  
binary  name fun assoc = Infix   (do{ string name; spaces ;return fun }) assoc

prefix :: String -> (a -> a) -> Operator String () Identity a 
prefix  name fun       = Prefix  (do{ string name; spaces; return fun })

postfix :: String -> (a -> a) -> Operator String () Identity a  
postfix name fun       = Postfix (do{ spaces;string name; spaces; return fun }) 

seqOp :: Operator String () Identity APeg  
seqOp = Infix   (try (lookAhead apegTerm >> return Seq)) AssocLeft


-- *************************************************
-- **  APEG Attribute Expression language Parser  **  
-- *************************************************

strAttExp :: Parser Expr
strAttExp = do char '"'
               s <- many (noneOf "\n\"")
               char '"'
               return (Str s)

varAttExp :: Parser Expr
varAttExp = ident >>= return.EVar

epsAttExp :: Parser Expr
epsAttExp = string "Eps" >> return Epsilon

intAttExp :: Parser Expr
intAttExp = many1 digit >>= return.(ILit).read

exprAttTerm :: Parser Expr
exprAttTerm = spaces >>
              (intAttExp <|>
               epsAttExp <|>
               varAttExp <|>
               strAttExp <|>
               (try metaLam) <|>
               (try metaLit) 
               
               )
              

exprTable :: OperatorTable String () Identity Expr
exprTable   = [ [binary "+" (BinOp 0) AssocLeft],
                [binary "<:" (Union) AssocLeft]

              ]

              
apegAttExp :: Parser Expr          
apegAttExp = buildExpressionParser exprTable (exprAttTerm >>= \t -> spaces >> return t) 
             <?> "APEG Expression"
         
         
-- ******************************************************
-- **  APEG Meta APEG Expression language Parser       **  
-- ******************************************************

metaLam :: Parser Expr 
metaLam = char '#' >> lambda >>= (\_ -> return $ MetaPeg MkLambda )

metaLit :: Parser Expr
metaLit = char '#' >> literal >>= (\(Lit s) -> return $ MetaPeg (MkLit (Str s)) )



-- ******************************************************
-- **  APEG Meta Attribute Expression language Parser  **  
-- ******************************************************



metaEps :: Parser Expr
metaEps = char '#' >> epsAttExp  >>= (\_ -> return $ MetaExp MEpsilon )



{-
attrTerm :: Parser Expr
attrTerm = do -}



