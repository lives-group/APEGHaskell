
module APEG.ASTSamples.FooExt where
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
identifier =   Ann Flat (Seq lower (Kle (Alt lower digit )))

rid :: APeg
rid =  Ann Flat (Seq (Alt lower upper) (Kle $ (alts [lower,upper,digit])))

reswrd :: String -> APeg
reswrd s = Lit s

fator :: APeg
fator = Alt num identifier

wht :: APeg
wht = chrs ['\n',' ','\t']

whts :: APeg
whts = Ann Dump (Kle wht)

whts1 :: APeg
whts1 = Ann Dump (many1 wht)

semic :: APeg
semic = Ann Dump (Lit ";")

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

microSugar :: ApegGrm
microSugar = [rulestart,rulex, ruler1 ]


rulestart :: ApegRule
rulestart = ApegRule "start"
                     [(TyLanguage,"g")]
                     []
                     ( alts[ seqs [ Lit "ext",
                                    Update [("ex",MkRule (Str "x")
                                                         [(MetaExp MkTyLanguage,Str "g")]
                                                         []
                                                         (MetaPeg $ MkLit (Str "some")) )],
                                    NT "r1" [Union g (EVar "ex")] []
                                   ],
                            seqs [Lit "noExt", NT "r1" [g] []]
                            ]
                     )

ruler1 :: ApegRule
ruler1  = ApegRule "r1"
                    [(TyLanguage,"g")]
                    []
                    (alts [ NT "x" [g] [],
                            Lit "none"
                           ])

rulex :: ApegRule
rulex = ApegRule "x"
                 [(TyLanguage,"g")]
                 []
                 (Not Lambda)


runExtstr :: String -> IO ()
runExtstr s = runGrammar  microSugar [] s

runExt :: FilePath -> IO ()
runExt =  runFile (runGrammar microSugar [])

acceptExt :: String -> IO ()
acceptExt = runFile (runAccept  microSugar [])

debugExt :: String -> IO ()
debugExt = runFile (debugRun  microSugar [])


extListRules :: IO ()
extListRules = mapM_ (putStrLn.sig2str) (signatures microSugar)

