
module APEG.ASTSamples.MicroSugar where
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
import APEG.DSL

idLit :: APeg
idLit = Kle $ chrs "()*+,-./0123456789:;<=>!?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"

semic :: APeg
semic = Ann Dump (lit ";")

rid :: APeg
rid =  Ann Flat (Seq (Alt lower upper) (star $ (alts [lower,upper,digit])))

--
-- g :: Expr
-- g = v "g"
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
microSugar = [ruleProg,
              ruleNewSyn,
              ruleRule,
              rulePattern,
              ruleSeq,
              ruleExtStmt,
              ruleKFator,
              rulepFator,
              ruleExpr,
              ruleBlock,
              ruleStmt,
              ruleCExpr,
              ruleFator,
              ruleID,
              whiteRule,
              whiteRule1]


ruleProg :: ApegRule
ruleProg = rule "prog"
                    ["g" .:: tLang]
                    []
                    (seqs [ "sigma" .<. mapvals [(Str "0",eps)],
                            star (call "newSyn" [g,(v "sigma")] ["sigma"]),
                            star1 $ (call "extBlock" [g,v "sigma"] []) ./.
                                    (call "block" [g] [])
                          ])

ruleNewSyn :: ApegRule
ruleNewSyn = rule "newSyn"
                      ["g" .:: tLang,
                       "s" .:: (tMap tGrm)]
                      [v "s" .:: (tMap tGrm)]
                      (seqs ["sigma" .<. eps,
                             lit "define",
                             wht,
                             "n" .=.  rid,
                             whts,
                             lit "{",
                             whts,
                             star $ seqs [call "rule" [g] ["r"],
                                          "sigma" .<. (v "sigma" <+: v "r")],
                             whts,
                             lit "}",
                             whts,
                             "s" .<. mapins (v "s") (v "n") (v "sigma")
                            ])

ruleRule :: ApegRule
ruleRule = rule "rule"
                     ["g" .:: tLang]
                     [ mkRule (v "nt") [(mX mkTLang, str "g")] [] (v "p") .:: tGrm]
                     (seqs [whts,
                            "nt" .=. rid,
                            whts,
                            lit "->",
                            whts,
                            call "pattern" [g] ["p"],
                            whts,
                            lit ";"
                           ] )

rulePattern :: ApegRule
rulePattern = rule "pattern"
                        ["g" .:: tLang]
                        [v "pe" .:: tMAPeg]
                        (seqs [ call "pseq" [g] ["pe"],
                                star (seqs [whts,
                                           lit "/",
                                           whts,
                                           call "pseq" [g] ["pd"],
                                           "pe" .<. aX  ( v "pe" |/| v "pd" ) ])
                              ])
--
ruleSeq :: ApegRule
ruleSeq = rule "pseq"
                   ["g" .:: tLang]
                   [v "pe" .:: tMAPeg]
                   (seqs [call "pKFator" [g] ["pe"],
                          star $ seqs [
                                       whts,
                                       call "pKFator" [g] ["pd"],
                                       "pe" .<. aX (v "pe" |:| v "pd")
                                       ]])

ruleKFator :: ApegRule
ruleKFator
 = rule "pKFator" ["g" .:: tLang] [v "pf" .:: tMAPeg]
        ( call "pFator" [v "g"] ["kf"] .:. whts .:.
           ( (lit "*" .:. "pf" .<. aX (mXStar (v "kf")) ) ./.
             (lam     .:. "pf" .<. v "kf") )
        )


rulepFator :: ApegRule
rulepFator = rule "pFator" ["g" .:: tLang] [v "pf" .:: TyMetaAPeg]
                     (alts [seqs [ lit "!", whts, call "pFator" [g] ["f"], "pf" .<. aX (mXNot (v "f") ) ],
                            Ann Flat (seqs [ lit "\"",
                                             "f" .=. idLit,
                                             lit "\"",
                                             "pf" .<. aX (mXLit (v "f"))]),
                            seqs [ lit "(",
                                   whts,
                                   call "pattern" [g] ["pf"],
                                   whts,
                                   lit ")"],
                            seqs ["ntName" .=. rid,
                                  "pf" .<. aX (mXCall (v "ntName") [mX (mkVar "g")] [] )],
                            seqs [lit "$",
                                  "pf" .<. aX  mkLambda]

                           ])


ruleExtStmt :: ApegRule
ruleExtStmt = rule "extBlock"
                       ["g" .:: tLang, "sigma" .:: (tMap tGrm)]
                       []
                       (seqs [ whts,
                               lit "syntax",
                               "nwSyn" .<. eps,
                               whts,
                               "n" .=. rid,
                               "nwSyn" .<. (((v "sigma") .!. (v "n")) <+: (v "nwSyn")),
                               star (seqs [whts,
                                          lit ",",
                                          whts,
                                          "n" .=. rid,
                                          "nwSyn" .<. (((v "sigma") .!. (v "n")) <+: (v "nwSyn"))
                                         ]),
                               call "block" [g <+: (v "nwSyn")] []
                               ] )


ruleBlock :: ApegRule
ruleBlock = rule "block"
                     ["g" .:: tLang]
                     []
                     (seqs [whts,
                            lit "{",
                            star1 (call "stmt" [g] []),
                            whts,
                            lit "}"])

ruleStmt :: ApegRule
ruleStmt = rule "stmt"
                     ["g" .:: tLang]
                     []
                     ( Seq whts
                           (alts [seqs [lit "print(", whts, call "expr" [g] [], whts, Ann Dump (lit ")"), whts, semic],
                                  seqs [lit "read(", whts, identifier , whts, Ann Dump ( lit ")"), whts, semic],
                                  seqs [lit "if(", call "cexpr" [g] [] ,whts, Ann Dump (lit ")"), whts ,call "block" [g] []],
                                  seqs [lit "loop(", call "cexpr" [g] [] ,whts,Ann Dump ( lit ")"), whts ,call "block" [g] []],
                                  seqs [identifier, whts, lit ":=" ,whts, call "expr" [g] [] , whts, semic]
                                ]))

ruleExpr :: ApegRule
ruleExpr = rule "expr"
                    ["g" .:: tLang]
                    []
                    (seqs [call "cexpr" [g] [],whts, star $ seqs [chrs ['+','-'], whts,  call "cexpr" [g] []]])

ruleCExpr :: ApegRule
ruleCExpr = rule "cexpr"
                     ["g" .:: tLang]
                     []
                     (seqs [call "fator" [g] [], whts, star $ seqs [chrs ['<','='],whts,  call "fator" [g] [] ]])

ruleFator :: ApegRule
ruleFator = rule "fator"
                     ["g" .:: tLang]
                     []
                     (alts [ seqs [lit "(", whts, call "expr" [g] [], whts, lit ")"],
                             lit "true",
                             lit "false",
                             Ann Flat num,
                             identifier
                           ])
ruleID :: ApegRule
ruleID = rule "identifier"
                     ["g" .:: tLang]
                     []
                     identifier

whiteRule :: ApegRule
whiteRule = rule "whites"
                     ["g" .:: tLang]
                     []
                     (whts)


whiteRule1 :: ApegRule
whiteRule1 = rule "whites1"
                     ["g" .:: tLang]
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

