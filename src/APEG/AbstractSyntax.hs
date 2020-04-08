 
module APEG.AbstractSyntax where

import Control.Monad
import Data.Char 
import Data.List
import qualified Data.Map as M
import Test.QuickCheck


type NonTerminal = String

type Var = String

type ApegGrm = [ApegRule]
 
-- ApegRule  (Name or String) (Inherited Syntatical Parameters)
data ApegRule = ApegRule NonTerminal [(Type,Var)] [(Type,Expr)] APeg deriving Show

data APeg = Lambda                            -- ^ The Lambda Literal
         | Lit String                         -- ^ The String Literal
         | NT NonTerminal [Expr] [Var]
         | Kle APeg
         | Not APeg
         | Seq APeg APeg
         | Alt APeg APeg
         | AEAttr [(Var,Expr)]                           -- The list of attributions of expressions to varaibales
         | Bind Var APeg
         deriving (Show,Eq)

data Expr = Str String                                   -- string literal
          | Epsilon                                      -- The empty grammar
          | EVar Var                                     -- variable 
          | MetaPeg MAPeg                                -- meta level PEG
          | MetaExp MExpr                                -- meta level Expr
          | Union Expr Expr                              -- Uniao Language Language
          | ExtRule Expr Expr Expr                       -- ExtRule  Grammar RuleName Apeg
          | MkRule Expr [(Expr,Expr)] [(Expr, Expr)] Expr -- new non terminal creation
          | MapLit [(Expr,Expr)]                          -- map literal
          | MapIns Expr Expr Expr                        -- Map insertion method: m[s / v] means MapIns m s v 
          | MapAccess Expr Expr                          -- Map Access method: m[s] = MapAccess m s
          deriving (Show,Eq)

          
data MExpr = MVar Expr
           | MEpsilon
           | MStr Expr
           | MUnion Expr Expr 
           | MMapLit [(Expr,Expr)]
           | MMapIns Expr Expr Expr
           | MMapAcces Expr Expr
           | MkTyStr
           | MkTyLanguage
           | MkTyMap Expr
           deriving (Show, Eq)
           
 -- Combinators for dynamically building PEGS 
data MAPeg = MkLambda 
           | MkLit Expr
           | MkCal Expr [Expr] [Expr]
           | MkKle Expr 
           | MkNot Expr
           | MkSeq Expr Expr
           | MkAlt Expr Expr
           | MkAE [(Expr,Expr)] 
           deriving (Show,Eq)
    
data Type = TyStr
          | TyAPeg
          | TyGrammar
          | TyMap Type
          | TyRule [Type] [Type]
          | TyMetaAPeg
          | TyMetaExp
          | TyMetaType
          | TyLanguage -- This type is to be attributed to Grammar whose all rules are correct.
          deriving (Show, Eq)

          
pprintType :: Type -> String
pprintType TyStr  = "Str"
pprintType TyAPeg = "APeg"
pprintType TyGrammar = "Grammar"
pprintType TyMetaAPeg = "#APeg"
pprintType TyMetaExp = "#Exp"
pprintType TyMetaType = "#Type"
pprintType TyLanguage = "Lang"
pprintType (TyMap t) = "[" ++ (pprintType t) ++"]"
pprintType (TyRule inh syn) = "(" ++ (concat $ intersperse "," $ map pprintType inh) ++ ") -> (" ++ (concat $ intersperse "," $ map pprintType syn) ++ ")" 


-- =================== AST Manipulation Utilities =================== --
                                         
fetch :: ApegGrm -> String -> Maybe ApegRule
fetch [] _ = Nothing
fetch (x@(ApegRule nt _ _ _  ):xs) s
    | nt == s = Just x
    | otherwise = fetch xs s


mkAltBody :: APeg -> APeg -> APeg
mkAltBody x y = Alt x y

grmExtRule :: ApegGrm -> NonTerminal -> APeg -> ApegGrm
grmExtRule [] _ _ = []
grmExtRule (x@(ApegRule nt inh syn body):xs) nt' newAlt 
    | nt == nt' = (ApegRule nt inh syn (mkAltBody body newAlt)):xs
    | otherwise = x: grmExtRule xs nt' newAlt
    
isMetaType :: Type -> Bool
isMetaType TyMetaAPeg = True
isMetaType (TyMetaExp) = True
isMetaType (TyMetaType) = True
isMetaType _   = False

joinRules ::  ApegGrm -> ApegGrm -> ApegGrm
joinRules g [] = g
joinRules g (x:xs) = joinRules (grmAddRule g x) xs

grmAddRule :: ApegGrm -> ApegRule -> ApegGrm
grmAddRule [] r = [r]
grmAddRule (x@(ApegRule nt inh syn body):xs) r@(ApegRule nt' inh' syn' body')
    | (nt == nt') && (inh == inh') && (syn == syn') = ApegRule nt inh syn (mkAltBody body body'):xs
    | (nt == nt') && ((inh /= inh') || (syn /= syn')) = error ("Conflicting definitions of " ++ nt ++ " when composing languages.")
    | (nt /= nt') = x: grmAddRule xs r 

-- =================== Show Utilities =================== --

prec :: APeg -> Int
prec  Lambda = 0
prec (Lit _) = 0
prec (Kle _) = 1
prec (Not _) = 1
prec (Seq _ _) = 2
prec (Alt _ _) = 3

parensStr :: Bool -> String -> String
parensStr True s  = "(" ++ s ++ ")" 
parensStr False s = s

isPegAlt :: APeg -> Bool
isPegAlt (Alt _ _) = True
isPegAlt _       = False

isPegSeq :: APeg -> Bool 
isPegSeq (Seq _ _) = True
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
