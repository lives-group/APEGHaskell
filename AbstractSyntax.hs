 
module AbstractSyntax where

import Data.List
import qualified Data.Map as M

type NonTerminal = String
type Var = String

type ApegGrm = [ApegRule]
 

 
-- ApegRule  (Name or String) (Inherited Syntatical Parameters)
data ApegRule = ApegRule NonTerminal [(Type,Var)] [Expr] APeg deriving Show

data APeg = Lambda
         | Lit String
         | NT NonTerminal [Expr] [Expr]
         | Kle APeg
         | Not APeg
         | Seq [APeg]
         | Alt [APeg]
         | AEAttr [(Var,Expr)] -- The list of attributions of expressions to varaibales
         deriving Show
              
data Expr = Str String 
          | EmptyMap  
          | MetaPeg MAPeg
          | EVar Var
          | Mp [(String,Expr)] 
          | MapIns Expr String Expr -- Map insertion method
          | MapAcces Expr Expr         -- Map Access method
          | ExtRule NonTerminal MAPeg
          | MkRule NonTerminal [(Type,Var)] [MExpr] MAPeg
          deriving Show
          

data MAPeg = MkLambda 
           | MkCal NonTerminal [MExpr] [MExpr]
           | MkKle MAPeg 
           | MkNot MAPeg
           | MkSeq [MAPeg]
           | MkAlt [MAPeg]
           | MkAE [(Var,MExpr)] 
           deriving Show
          
data MExpr = MkStr String
           | MkVar Var
           | MkEmptyMap
           | MkMp [(String,MExpr)]
           | MkMapIns MExpr String MExpr
           | MkMapAcces MExpr MExpr
           deriving Show

data Type = TyStr
          | TyMetaCons Type
          | TyLanguage -- This type is to be attributed to Grammar whose all rules are correct.
          | TyMap Type
          deriving (Show, Eq)


fetch :: ApegGrm -> String -> ApegRule
fetch (x@(ApegRule nt _ _ _  ):xs) s
    | nt == s = x
    | otherwise = fetch xs s
    

prec :: APeg -> Int
prec  Lambda = 0
prec (Lit _) = 0
prec (Kle _) = 1
prec (Not _) = 1
prec (Seq _) = 2
prec (Alt _) = 3

parensStr :: Bool -> String -> String
parensStr True s  = "(" ++ s ++ ")" 
parensStr False s = s

isPegAlt :: APeg -> Bool
isPegAlt (Alt n) = True
isPegAlt _       = False

isPegSeq :: APeg -> Bool
isPegSeq (Seq _) = True
isPegSeq _       = False

parensAssoc :: APeg -> APeg -> Bool
parensAssoc encl inner 
    | (isPegAlt encl) && (isPegSeq inner) = True
    | (isPegSeq encl) && (isPegAlt inner) = True
    | otherwise = False
    
-- instance Show APeg where
--      showsPrec d (Lit l)        = \s -> ('\'':l ++ ['\'']) ++ s
--      showsPrec d Lambda         = \s -> ("\x03BB") ++ s
--      showsPrec d (NT n xs ys)   = \s ->n ++ s
--      showsPrec d c@(Kle p)      = (parensStr (prec c > d) ).(\s -> s ++ "*").(showsPrec (prec c) p) 
--      showsPrec d c@(Not p)      = (parensStr (prec c > d) ).(showString "!").(showsPrec (prec c) p)
--      showsPrec d c@(AEAttr xs)  = (showString "{").(showsPrec (prec c) p).(showString "}")
--      showsPrec d c@(Seq xs)     = (parensStr (prec c > d) ).(foldr1 (.) (map (showsPrec (prec c)) xs)) 
--      showsPrec d c@(Alt xs)     = (parensStr (prec c > d) ).(foldr1 (\c d -> c.(showString "|").d) (map (showsPrec (prec c)) xs))
--      show p                     = showsPrec 3 p ""
