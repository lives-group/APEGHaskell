
module APEG.ASTSamples.MicroSugar2 where
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
microSugar = [ruleProg,ruleNewSyn,ruleRule,
              rulePattern,ruleSeq,ruleExtStmt,
              ruleKFator,rulepFator,ruleExpr,
              ruleBlock,ruleStmt,ruleCExpr,
              ruleFator, ruleFatorExt, ruleID,whiteRule,whiteRule1]


ruleProg :: ApegRule
ruleProg = ApegRule "prog"
                    [(TyLanguage,"g")]
                    []
                    (seqs [ Update [("sigma",MapLit [(Str "0",Epsilon)])],
                            Kle (NT "newSyn" [g,(EVar "sigma")] ["sigma"]),
                            many1 $ Alt (NT "rStmt" [g,EVar "sigma"] []) (NT "block" [g] [])
                          ])

ruleNewSyn :: ApegRule
ruleNewSyn = ApegRule "newSyn"
                      [(TyLanguage,"g"),
                       (TyMap TyGrammar,"s")]
                      [(TyMap TyGrammar,EVar "s")]
                      (seqs [Update [("lan",Epsilon)],
                             Lit "define",
                             wht,
                             Bind "n" rid,
                             whts,
                             Lit "{",
                             whts,
                             Kle $ seqs [NT "rule" [g] ["r"],
                                         Update [("lan", Union (EVar "lan") (EVar "r"))]],
                             whts,
                             Lit "}",
                             whts,
                             Update [("s",MapIns (EVar "s") (EVar "n") (EVar "lan"))]
                            ])

ruleRule :: ApegRule
ruleRule = ApegRule "rule"
                    [(TyLanguage,"g")] -- Tem um problema aqui ! A sintaxe não tem meios para falar de tipos !
                    [(TyGrammar,MkRule (EVar "nt") [(MetaExp MkTyLanguage,Str "g")] [] (EVar "p"))]
                    (seqs [ whts,
                            Bind "nt" rid,
                            whts,
                            Lit "->",
                            whts,
                            NT "pattern" [g] ["p"],
                            whts,
                            Lit ";"
                           ] )

rulePattern :: ApegRule
rulePattern = ApegRule "pattern"
                        [(TyLanguage,"g")]
                        [(TyMetaAPeg,EVar "pe")]
                        (seqs [ NT "pseq" [g] ["pe"],
                                Kle (seqs [whts,
                                           Lit "/",
                                           whts,
                                           NT "pseq" [g] ["pd"],
                                           Update [("pe",MetaPeg $ MkAlt (EVar "pe") (EVar "pd"))]])

                              ])
--
ruleSeq :: ApegRule
ruleSeq = ApegRule "pseq"
                        [(TyLanguage,"g")]
                        [(TyMetaAPeg,EVar "pe")]
                        (seqs [NT "pKFator" [g] ["pe"],
                               Kle $ seqs [
                                           whts,
                                           NT "pKFator" [g] ["pd"],
                                           Update [("pe",MetaPeg $ MkSeq (EVar "pe") (EVar "pd"))]
                              ]])

ruleKFator :: ApegRule
ruleKFator = ApegRule "pKFator"
                     [(TyLanguage, "g")]
                     [(TyMetaAPeg,  EVar "pf")]
                     (seqs [ NT "pFator" [g] ["kf"],
                             whts,
                             alts [ Seq (Lit "*") (Update[("pf", MetaPeg $ MkKle (EVar "kf"))]),
                                    Seq (Lambda)  (Update[("pf", EVar "kf")])]])


rulepFator :: ApegRule
rulepFator = ApegRule "pFator"
                     [(TyLanguage, "g")]
                     [(TyMetaAPeg,  EVar "pf")]
                     (alts [seqs [ Lit "!",
                                   whts,
                                   NT "pFator" [g] ["f"],
                                   Update[("pf",MetaPeg $ MkNot (EVar "f"))] ],
                            Ann Flat (seqs [ Lit "\"",
                                             Bind "f" lit,
                                             Lit "\"",
                                             Update[("pf",MetaPeg $ MkLit (EVar "f"))]]),
                            seqs [ Lit "(",
                                   whts,
                                   NT "pattern" [g] ["pf"],
                                   whts,
                                   Lit ")"],
                            seqs [Bind "ntName" rid,
                                  Update[("pf",MetaPeg $ MkCal (EVar "ntName") [MetaExp (MVar $ Str "g")] [])]],
                            seqs [Lit "$",
                                  Update[("pf",MetaPeg $ MkLambda)]]

                           ])

ruleExtStmt :: ApegRule
ruleExtStmt = ApegRule "rStmt"
                       [(TyLanguage,"g"),(TyMap TyGrammar,"sigma")]
                       []
                       (seqs [ whts,
                               Lit "syntax",
                               Update [("nwSyn",Epsilon)],
                               whts,
                               (Bind "n" rid),
                               Update [("nwSyn",Union (MapAccess (EVar "sigma") (EVar "n")) (EVar "nwSyn"))],
                               Kle (seqs [whts,
                                          Lit ",",
                                          whts,
                                          Bind "n" rid,
                                          Update [("nwSyn",Union (MapAccess (EVar "sigma") (EVar "n")) (EVar "nwSyn"))]
                                         ]),
                               NT "block" [Union g (EVar "nwSyn")] []
                               ] )


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
                     (alts [ NT "fatorExt" [g] [],
                             seqs [Lit "(", whts, NT "expr" [g] [], whts, Lit ")"],
                             Lit "true",
                             Lit "false",
                             Ann Flat num,
                             identifier
                           ])

ruleFatorExt :: ApegRule
ruleFatorExt = ApegRule "fatorExt"
                     [(TyLanguage,"g")]
                     []
                     (Not Lambda)

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

runMS :: FilePath -> IO ()
runMS =  runFile (runGrammar microSugar [])

acceptMS :: String -> IO ()
acceptMS = runFile (runAccept  microSugar [])

debugMS :: String -> IO ()
debugMS = runFile (debugRun  microSugar [])


msListRules :: IO ()
msListRules = mapM_ (putStrLn.sig2str) (signatures microSugar)

