
module APEG.ASTSamples.DataDependentLanguage where
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


-- ========== Some Utilitary Functions For Debugging ============= --

sig :: ApegRule -> (String,[(Type, Var)],[(Type,Expr)])
sig ( ApegRule n xs ys _) = (n,xs,ys)

signatures :: ApegGrm -> [(String,[(Type, Var)],[(Type,Expr)])]
signatures = map sig

sig2str :: (String,[(Type, Var)],[(Type,Expr)]) -> String
sig2str (s, xs, ys) = s ++ " : " ++
                      (concat $ intersperse "," $ map (pprintType.fst) xs) ++ " -> " ++
                      (concat $ intersperse "," $ map (pprintType.fst) ys)

-- ========== An Auxiliar Definition ============= --

lowLetter :: APeg
lowLetter = chrs "abcdefghijklmnopqrstuvwxyz"

-- ========== Data Dependent Language Grammar ============= --

ddl :: ApegGrm
ddl = [ruleStart, ruleDigit,ruleLetters]


ruleStart :: ApegRule
ruleStart = rule "start"
                    ["g" .:: tLang]
                    []
                    (seqs [ "n" .<. int 0,
                            call "digit" [g] ["n"],
                            lit "[",
                            star ( seqs [ ((int 0) |<| (v "n")) |?| lowLetter,
                                          "n" .<. ((v "n") |-| (int 1))] ),
                            ((int 0) |=| (v "n")) |?| lit "]"
                           ])



ruleDigit :: ApegRule
ruleDigit = rule "digit"
                      ["g" .:: tLang]
                      [v "s" .:: (tInt)]
                      (alts [lit "1" .:. "s" .<. int 1,
                             lit "2" .:. "s" .<. int 2,
                             lit "3" .:. "s" .<. int 3,
                             lit "4" .:. "s" .<. int 4,
                             lit "5" .:. "s" .<. int 5,
                             lit "6" .:. "s" .<. int 6,
                             lit "7" .:. "s" .<. int 7,
                             lit "8" .:. "s" .<. int 8,
                             lit "9" .:. "s" .<. int 9])


runDDL :: String -> IO ()
runDDL =  (runGrammar ddl [])

msListRules :: IO ()
msListRules = mapM_ (putStrLn.sig2str) (signatures ddl)

