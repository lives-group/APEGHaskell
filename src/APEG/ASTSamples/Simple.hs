
module APEG.ASTSamples.Simple where
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import APEG.Interpreter.APEGInterp
import APEG.Interpreter.MonadicState
import APEG.Interpreter.MaybeString
import APEG.Interpreter.State
import Control.Monad.State.Lazy
import APEG.AbstractSyntax
import APEG.Interpreter.State
import APEG.TypeSystem
import APEG.PlayGround
import Data.String


alts :: [APeg] -> APeg
alts  = foldl1 Alt

seqs :: [APeg] -> APeg
seqs = foldl1 Seq

chrs :: [Char] -> APeg
chrs = alts.(map (Lit.(:[])))

many1 :: APeg -> APeg
many1 p = Seq p (Kle p)

digit :: APeg
digit = chrs ['0'..'9']

num :: APeg
num = many1 digit

upper :: APeg
upper = chrs ['A'..'Z']

lower :: APeg
lower = chrs ['a'..'z']

lit :: APeg
lit = Kle $ chrs "()*+,-./0123456789:;<=>!?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"

identifier :: APeg
identifier =  (Seq lower (Kle (Alt lower digit )))

rid :: APeg
rid = Seq (Alt lower upper) (Kle $ (alts [lower,upper,digit]))

reswrd :: String -> APeg
reswrd s = Lit s

fator :: APeg
fator = Alt num identifier

wht :: APeg
wht = chrs ['\n',' ','\t']

whts :: APeg
whts = Ann Flat (Kle wht)

whts1 :: APeg
whts1 = many1 wht


g :: Expr
g = EVar "g"

-- ========== MicroSugar APEG Grammar ============= --

sig :: ApegRule -> (String,[(Type, Var)],[(Type,Expr)])
sig ( ApegRule n xs ys _) = (n,xs,ys)

signatures :: ApegGrm -> [(String,[(Type, Var)],[(Type,Expr)])]
signatures = map sig

sig2str :: (String,[(Type, Var)],[(Type,Expr)]) -> String
sig2str (s, xs, ys) = s ++ " : " ++
                      (concat $ intersperse "," $ map (pprintType.fst) xs) ++ " -> " ++
                      (concat $ intersperse "," $ map (pprintType.fst) ys)


simple :: ApegGrm
simple = [ruleTest, ruleTest2]


ruleTest :: ApegRule
ruleTest = ApegRule "test"
                    [(TyLanguage,"g")]
                    []
                    (seqs [Lit "a", Lit ":", whts1, (Kle (Alt (Lit "a") (Lit "b"))) ])

ruleTest2 :: ApegRule
ruleTest2 =  ApegRule "test2"
                    [(TyLanguage,"g")]
                    []
                    (Kle (Alt (Lit "a") (Lit "b")))


runS :: FilePath -> IO ()
runS =  runFile (runGrammar simple [])

acceptS :: String -> IO ()
acceptS = runFile (runAccept  simple [])

debugS :: String -> IO ()
debugS = runFile (debugRun  simple [])


sListRules :: IO ()
sListRules = mapM_ (putStrLn.sig2str) (signatures simple)

