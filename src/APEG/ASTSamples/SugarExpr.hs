module APEG.ASTSamples.SugarExpr where

import APEG.Interpreter.APEGInterp
import APEG.Interpreter.MonadicState
import APEG.Interpreter.MaybeString
import APEG.Interpreter.State
import Control.Monad.State.Lazy
import APEG.AbstractSyntax
import APEG.Interpreter.State
import APEG.TypeSystem
import APEG.DSL
import APEG.PlayGround


{-

prog<g :: Lang> -> (defExtSet<g,sugarList> 
                | expr<g,sugarList>+
                
expr<g :: Lang, sugarList :: [Grammar]> -> fator<g,SugarList> (Expr1<g,sugarList> | termo<g,sugarList>)*

spot -> ?{ false}

expr1<g :: Language, sugarList :: [Grammar]> -> "+" expr1<g,sugarList>
                                              | "-" expr1<g,sugarList>
termo<g :: Lang, sugarList :: [Grammar]> -> "*" termo<g :: Lang, sugarLis :: [Grammar]> 

fator<g :: Lang, SugarList :: [Grammar]>



-}


rid :: APeg
rid = Seq (Alt lower upper) (Kle $ (alts [lower,upper,digit]))

sugarExpr :: ApegGrm
sugarExpr = [ruleProg,ruleExtSet,ruleRule,rulePattern,ruleSeq,
             ruleKFator, whiteRule]

              
ruleProg :: ApegRule
ruleProg 
 = rule "prog" ["g" .:: tLang] [] 
         (seqs ["sigma" .<. (mapvals [(str "0",Epsilon)]),  
                star (call "defExtSet" [v "g"] ["sugarList"] ),
                star1 ( call "expr" [v "g", v "sugarList"] []),
                whts])

ruleExtSet :: ApegRule
ruleExtSet 
 = rule "defExtSet" ["g" .:: tLang] [v "s" .:: tMap tGrm] 
        ( "lan" .<. eps .:. 
           lit "ext" .:. wht .:. "n" .=. rid .:. whts .:. lit "{" .:. 
           whts .:. (star (call "rule" [v "g"] ["r"] .:. "lan" .<. (v "lan" <+: v "r") ) ) .:.
           whts .:. lit "}" .:. "s" .<. ins (v "s") (v "n") (v "lan")
        )

ruleRule :: ApegRule
ruleRule
 = rule "rule" ["g" .:: tLang] [ mkRule (v "nt") [(mX mkTLang, str "g")] [] (v "p") .:: tGrm] 
        ( whts .:. "nt" .=. rid .:. whts .:. lit "->" .:. whts .:. 
          call "pattern" [v "g"] ["p"] .:. whts .:. lit ";")
                    
rulePattern :: ApegRule
rulePattern 
 = rule "pattern" ["g" .:: tLang] [v "pe" .:: tMAPeg]
        ( call "pseq" [v "g"] ["pe"] .:. 
          star (whts .:. lit "/" .:. whts .:. call "pseq" [v "g"] ["pd"] .:. 
                "pe" .<. aX  ( v "pe" |/| v "pd" )))

ruleSeq :: ApegRule
ruleSeq 
 = rule "pseq" [ "g" .:: tLang] [v "pe" .:: tMAPeg]
        (call "pKFator" [v "g"] ["pe"] .:. 
         star (whts .:. call "pKFator" [v "g"] ["pd"] .:. "pe" .<. aX (v "pe" |:| v "pd") ))


ruleKFator :: ApegRule 
ruleKFator 
 = rule "pKFator" ["g" .:: tLang] [v "pf" .:: tMAPeg]
        ( call "pFator" [v "g"] ["kf"] .:. whts .:.
           ( (lit "*" .:. "pf" .<. aX (mXStar (v "kf")) ) ./. 
             (lam     .:. "pf" .<. v "kf") )
        )
        
        
whiteRule :: ApegRule 
whiteRule = rule "whites" ["g" .:: tLang,  "sigma" .:: tMap tGrm] [] whts


typeEXP :: [String]
typeEXP = simpleType sugarExpr

runEXP :: FilePath -> IO ()
runEXP =  runFile (runGrammar sugarExpr []) 
              
acceptEXP :: String -> IO ()
acceptEXP = runFile (runAccept  sugarExpr [])

debugEXP :: String -> IO ()
debugEXP = runFile (debugRun  sugarExpr [])

