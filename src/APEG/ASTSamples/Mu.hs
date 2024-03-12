module APEG.ASTSamples.Mu where
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
identifier =   (Seq lower (Kle (Alt lower digit )))

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
microSugar = [ruleBlock,
              ruleExpr,ruleStmt,ruleCExpr,
              ruleFator,ruleID,whiteRule,whiteRule1]


ruleBlock :: ApegRule
ruleBlock = ApegRule "block"
                     [(TyLanguage,"g")]
                     []
                     (seqs [whts,
                            Lit "{",
                            many1 (NT "stmt" [g] []),
                            whts,
                            Lit "}"])

ruleStmt :: ApegRule
ruleStmt = ApegRule "stmt"
                     [(TyLanguage,"g")]
                     []
                     ( Seq whts
                           (alts [seqs [Lit "print(", whts, NT "expr" [g] [], whts, Ann Dump (Lit ")"), whts, semic],
                                  seqs [Lit "read(", whts, identifier , whts, Ann Dump ( Lit ")"), whts, semic],
                                  seqs [Lit "if(", NT "cexpr" [g] [] ,whts, Ann Dump (Lit ")"), whts ,NT "block" [g] []],
                                  seqs [Lit "loop(", NT "cexpr" [g] [] ,whts,Ann Dump ( Lit ")"), whts ,NT "block" [g] []],
                                  seqs [identifier, whts, Lit ":=" ,whts, NT "expr" [g] [] , whts, semic]
                                ]))

ruleExpr :: ApegRule
ruleExpr = ApegRule "expr"
                    [(TyLanguage,"g")]
                    []
                    (seqs [NT "cexpr" [g] [],whts, Kle $ seqs [chrs ['+','-'], whts,  NT "cexpr" [g] []]])

ruleCExpr :: ApegRule
ruleCExpr = ApegRule "cexpr"
                     [(TyLanguage,"g")]
                     []
                     (seqs [NT "fator" [g] [], whts, Kle $ seqs [chrs ['<','='],whts,  NT "fator" [g] [] ]])

ruleFator :: ApegRule
ruleFator = ApegRule "fator"
                     [(TyLanguage,"g")]
                     []
                     (alts [ seqs [Lit "(", whts, NT "expr" [g] [], whts, Lit ")"],
                             Lit "true",
                             Lit "false",
                             num,
                             identifier
                           ])
ruleID :: ApegRule
ruleID = ApegRule "identifier"
                     [(TyLanguage,"g")]
                     []
                     identifier

whiteRule :: ApegRule
whiteRule = ApegRule "whites"
                     [(TyLanguage, "g")]
                     []
                     (whts)


whiteRule1 :: ApegRule
whiteRule1 = ApegRule "whites1"
                     [(TyLanguage, "g")]
                     []
                     (whts1)
