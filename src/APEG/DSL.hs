module APEG.DSL where

import APEG.AbstractSyntax
import qualified Data.Map as M
import Control.Monad.State.Lazy

lit :: String -> APeg
lit s = Lit s

lam :: APeg
lam = Lambda


(.=.) :: String -> APeg -> APeg
(.=.) s a = Bind s a 

(.<.) :: String -> Expr ->  APeg
(.<.) s e = Update [(s,e)]

rule :: String -> [(Type,Var)] -> [(Type,Expr)] -> APeg -> ApegRule
rule s inh syn body=  ApegRule s inh syn body

-- |Create an alternative from a list of alternatives. 
alts :: [APeg] -> APeg
alts  = foldl1 Alt

call :: String -> [Expr] -> [Var] -> APeg
call s es vs =  NT s es vs

(./.) :: APeg -> APeg -> APeg
(./.) l r = Alt l r
infixl 7 ./.

(.:.) :: APeg -> APeg -> APeg
(.:.) l r = Seq l r
infixl 8 .:.


-- |Create an alternative from a list of sequences.
seqs :: [APeg] -> APeg
seqs = foldl1 Seq

-- | Sugar sintax for a range of characters. Ex.: [abcd] = a | b | c | d
chrs :: [Char] -> APeg
chrs = alts.(map (Lit.(:[])))

-- | Sugar syntex for positive Klenne (+).
star1 :: APeg -> APeg
star1 p = Seq p (Kle p)  

-- | Sugar syntex for positive Klenne (+).
star :: APeg -> APeg
star p = (Kle p)  

v :: String -> Expr
v = EVar

int :: Int -> Expr
int n = ILit n

str :: String -> Expr
str = Str

eps :: Expr
eps = Epsilon

(|+|) :: Expr -> Expr -> Expr
(|+|) e1 e2  = BinOp 0 e1 e2
infixl 5 |+| 

(<+:) :: Expr -> Expr -> Expr
(<+:) e1 e2 = Union e1 e2

extend :: Expr ->  Expr -> Expr -> Expr
extend grm rule apeg = ExtRule grm rule apeg


npred :: APeg -> APeg
npred l = Not l

(|?|) :: Expr -> APeg -> APeg
(|?|) e p = Constr e p 

(.!.) ::  Expr -> Expr -> Expr
(.!.) m i = MapAccess m i
infixl 6 .!.

tInt :: Type
tInt = TyInt

tLang :: Type
tLang = TyLanguage

tGrm :: Type
tGrm = TyGrammar

tStr :: Type
tStr = TyStr

tAPeg :: Type
tAPeg = TyAPeg

tMAPeg :: Type 
tMAPeg = TyMetaAPeg

tMExpr :: Type
tMExpr = TyMetaExp

tMType :: Type
tMType = TyMetaType

tMap :: Type -> Type
tMap = TyMap

tRule :: [Type] -> [Type] -> Type
tRule xs ys = TyRule xs ys

(.::) :: a -> Type -> (Type,a)
(.::) x t = (t,x)
infixr 7 .::

mapvals :: [(Expr,Expr)] -> Expr
mapvals xs = MapLit xs



-- -------------------------------------------------------------------------
-- --                 APEG META EXPRESSION AND TERMS                      --
-- -------------------------------------------------------------------------

mkRule :: Expr ->  [(Expr,Expr)] -> [(Expr, Expr)] -> Expr -> Expr
mkRule e xs ys body = MkRule e xs ys body

mX :: MExpr -> Expr
mX e = MetaExp e

aX :: MAPeg -> Expr
aX m = MetaPeg m

mkTStr :: MExpr
mkTStr = MkTyStr

mkTInt :: MExpr
mkTInt = MkTyInt

mkTLang :: MExpr
mkTLang = MkTyLanguage

mkTGrm :: MExpr
mkTGrm = MkTyGrammar

mXTMap :: Expr -> MExpr
mXTMap e = MkTyMap e

mkInt :: Int -> MExpr
mkInt n = mXInt (ILit n)

mXInt :: Expr -> MExpr
mXInt e = MILit e

mkStr :: String -> MExpr
mkStr s = mXStr (Str s)

mXStr :: Expr -> MExpr
mXStr e = MStr e

mkVar :: String -> MExpr
mkVar s = MVar (Str s)

mXVar :: Expr -> MExpr
mXVar e = MVar e

mXmapvals :: [(Expr,Expr)] -> MExpr
mXmapvals xs = MMapLit xs

mXMapIns :: Expr -> Expr -> Expr -> MExpr
mXMapIns m e v = MMapIns m e v

mXMapAcc :: Expr -> Expr -> MExpr
mXMapAcc m i = MMapAcces m i

mkLambda :: MAPeg 
mkLambda = MkLambda

mkLit :: String -> MAPeg
mkLit s = MkLit (Str s)

mXLit :: Expr -> MAPeg 
mXLit e = MkLit e

mXCall :: Expr -> [Expr] -> [Expr] -> MAPeg
mXCall e xs ys = MkCal e xs ys

mXStar :: Expr -> MAPeg
mXStar e = MkKle e

mXNot :: Expr -> MAPeg
mXNot e = MkNot e

mXPredicate :: Expr -> Expr -> MAPeg -- MkConstr Expr Expr
mXPredicate e p = MkConstr e p

(|:|) :: Expr -> Expr -> MAPeg
(|:|) l r = MkSeq l r
infixr 7 |:|

(|/|) :: Expr -> Expr -> MAPeg
(|/|) l r = MkAlt l r
infixr 8 |/|

(.<@.) :: Expr -> Expr -> MAPeg
(.<@.) v e = MkAE [(v,e)]


-- -------------------------------------------------------------------------
-- --                 APEG UTILITARY DEFINITIONS                          --
-- -------------------------------------------------------------------------

-- | PEG for digits
digit :: APeg
digit = chrs ['0'..'9']

-- | PEG for numbers
num :: APeg
num = star1 digit

-- | PEG for upper letters
upper :: APeg
upper = chrs ['A'..'Z']

-- | PEG for lower letters
lower :: APeg
lower = chrs ['a'..'z']

identifier :: APeg
identifier = Seq lower (Kle (Alt lower digit ))

reswrd :: String -> APeg
reswrd s = Lit s

wht :: APeg 
wht = chrs ['\n',' ','\t']

whts :: APeg 
whts = Kle wht

whts1 :: APeg 
whts1 = star1 wht

g :: Expr
g = EVar "g"
