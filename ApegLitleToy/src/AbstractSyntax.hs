 
module AbstractSyntax where

import Control.Monad
import Data.Char 
import Data.List
import qualified Data.Map as M
import Test.QuickCheck


type NonTerminal = String
type Var = String

type ApegGrm = [ApegRule]


 
-- ApegRule  (Name or String) (Inherited Syntatical Parameters)
data ApegRule = ApegRule NonTerminal [(Type,Var)] [Expr] APeg deriving Show

data APeg = Lambda                            
         | Lit String
         | NT NonTerminal [Expr] [Var]
         | Kle APeg
         | Not APeg
         | Seq APeg APeg
         | Alt APeg APeg
         | AEAttr [(Var,Expr)]                          -- The list of attributions of expressions to varaibales
         | Bind Var APeg
         deriving Show

data Expr = Str String                                  -- string literal
          | EVar Var                                    -- variable 
          | MetaPeg MAPeg                               -- meta level PEG
          | MetaExp Expr                                -- meta level Expr
          | Union Expr Expr                             -- Uniao Language Language
          | ExtRule Expr Expr Expr                      -- ExtRule  Grammar RuleName Apeg
          | MkRule NonTerminal [(Type,Var)] [Expr] Expr -- new non terminal creation
          | MpLit [(String,Expr)]                       -- map literal
          | MapIns Expr Expr Expr                       -- Map insertion method: m[s / v] means MapIns m s v 
          | MapAccess Expr Expr                         -- Map Access method: m[s] = MapAccess m s
          deriving Show
          
 -- Combinators for dynamically building PEGS 
data MAPeg = MkLambda 
           | MkCal NonTerminal [Expr] [Expr]
           | MkKle Expr 
           | MkNot Expr
           | MkSeq Expr Expr
           | MkAlt Expr Expr
           | MkAE [(Var,Expr)] 
           deriving Show


data Type = TyStr
          | TyAPeg
          | TyMap Type
          | TyRule [Type] [Type]
          | TyMetaAPeg Type
          | TyMetaExp Type
          | TyLanguage -- This type is to be attributed to Grammar whose all rules are correct.
          deriving (Show, Eq)


data Value = VStr String
           | VMap (M.Map String Value)
           | VLan ApegGrm
           | VPeg APeg
           | VExp Expr
           | Undefined
           deriving Show

-- generators

type InhSize = Int  -- number of generated inherited attributes  
type SynSize = Int  -- number of generated synthesized attributes.
type Depth   = Int

-- generator of types. 

genType :: Depth -> InhSize -> SynSize -> Gen Type
genType d n m
  | d > 1 
    = frequency
      [
        (10, return TyStr)
      , (10, return TyAPeg)
      , (10, TyMap <$> genType (d - 1) n m) 
      , (50, TyRule <$> (replicateM n (genType (d - 1) n m)) <*>
                        (replicateM m (genType (d - 1) n m)))
      ]
   | otherwise
     = oneof [ return TyStr
             , return TyAPeg ]

-- generating statically typed exprs

type Gamma = M.Map String Type -- typing context

genStrLit :: Gen String
genStrLit = vectorOf 4 (suchThat (arbitrary :: Gen Char) isAlphaNum)

genExpr :: Depth -> Gamma -> Type -> Gen Expr
genExpr _ _ TyStr
  = Str <$> vectorOf 4 (suchThat (arbitrary :: Gen Char) isAlphaNum)
genExpr _ _ TyAPeg
  = undefined


-- generating apegs

genAPEG :: Depth -> Gen APeg
genAPEG d
  | d > 1     = undefined
  | otherwise = undefined

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
