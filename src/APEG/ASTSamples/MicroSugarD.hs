module APEG.ASTSamples.MicroSugarD where

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

anyChar :: APeg
anyChar = Kle $ chrs "()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"

rid :: APeg
rid = Seq (Alt lower upper) (Kle $ (alts [lower,upper,digit]))

fator :: APeg
fator = num ./. identifier

-- ========== MicroSugar APEG Grammar ============= --


microSugar :: ApegGrm
microSugar = [ruleProg,ruleNewSyn,ruleRule,rulePattern,ruleSeq,ruleExtStmt,
              ruleKFator,rulepFator,ruleExpr,ruleBlock,ruleStmt,ruleCExpr,
              ruleFator,whiteRule]

ruleProg :: ApegRule
ruleProg 
 = rule "prog" ["g" .:: tLang] [] 
         (seqs ["sigma" .<. (mapvals [(str "0",Epsilon)]),  
                star (call "newSyn" [v "g"] ["sigma"]),
                star1 ( call "extBlock" [v "g", v "sigma"] [] ./. call "block" [v "g"] [] ),
                whts])


ruleNewSyn :: ApegRule
ruleNewSyn 
 = rule "newSyn" ["g" .:: tLang] [v "s" .:: tMap tGrm] 
        ( "lan" .<. eps .:. 
           lit "define" .:. wht .:. "n" .=. rid .:. whts .:. lit "{" .:. 
           whts .:. (star (call "rule" [v "g"] ["r"] .:. "lan" .<. (v "lan" <+: v "r") ) ) .:.
           whts .:. lit "}" .:. "s" .<. mapvals [(v "n", v "lan")]
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
         

rulepFator :: ApegRule
rulepFator 
 = rule "pFator" ["g" .:: tLang] [v "pf" .:: tMAPeg]
        ( (lit "!" .:. whts .:. call "pFator" [v "g"] ["f"] .:. "pf" .<. aX (mXNot (v "f") ) ) ./.
          (lit "\"" .:. "f" .=. anyChar .:. lit "\"" .:. "pf" .<. aX (mXLit (v "f")) )  ./.
          (lit "(" .:. whts .:. call "pattern" [v "g"] ["pf"] .:. whts .:. lit ")")  ./.
          ("ntName" .=. rid .:. "pf" .<. aX (mXCall (v "ntName") [mX (mkVar "g")] [] ) )
        )
 
 
ruleExtStmt :: ApegRule 
ruleExtStmt
 = rule "extBlock" ["g" .:: tLang, "sigma" .:: tMap tGrm] [] 
        (whts .:. lit "syntax" .:. "nwSyn" .<. eps .:. whts .:.
         "n" .=. rid .:. "nwSyn" .<. ( ((v "sigma") .!. (v "n")) <+: (v "nwSyn")) .:.
         star ( whts .:. lit "," .:. "n" .=. rid .:. 
                "nwSyn" .<. ( ((v "sigma") .!. (v "n")) <+: (v "nwSyn")) 
              ) .:. 
         whts .:. 
         call "block" [(v "g") <+: (v "nwSyn")] []
        
        )


ruleBlock :: ApegRule
ruleBlock 
 = rule "block" ["g" .:: tLang] [] 
        (whts .:. lit "{" .:. star1 ( call "stmt" [v "g"] []) .:. 
         whts .:. lit "}"
        )

                            
ruleStmt :: ApegRule 
ruleStmt 
 = rule "stmt" ["g" .:: tLang] []
        ( whts .:. 
          ((lit "print(" .:. call "expr" [v "g"] [] .:. whts .:. lit ")" .:. whts .:. lit ";") ./.
           (lit "read(" .:. identifier .:. whts .:. lit ")" .:. whts .:. lit ";") ./.
           (lit "if(" .:. call "cexpr" [v "g"] [] .:. whts .:. lit ")" .:. whts .:. call "block" [v "g"] []) ./.
           (lit "loop(" .:. call "cexpr" [v "g"] [] .:. whts .:. lit ")" .:. whts .:. call "block" [v "g"] []) ./.
           (identifier .:. whts .:. lit ":=" .:. whts .:. call "expr" [v "g"] [] .:. whts .:. lit ";"))
        )

ruleExpr :: ApegRule         
ruleExpr 
 = rule "expr" ["g" .:: tLang] []
        ( call "cexpr" [v "g"] [] .:. whts .:. star ( chrs "+-" .:. whts .:. call "cexpr" [v "g"] []) )
                    
                    
ruleCExpr :: ApegRule
ruleCExpr
 = rule "cexpr" ["g" .:: tLang] []
        ( call "fator" [v "g"] []  .:. whts .:. 
          star (chrs "<=" .:. whts .:. call "fator" [v "g"] []))

ruleFator :: ApegRule 
ruleFator
 = rule "fator" ["g" .:: tLang] []
       ( (lit "(" .:. whts .:. call "expr" [v "g"] [] .:. whts .:. lit ")") ./.
         (lit "true") ./.
         (lit "false") ./.
         num ./.
         identifier)


whiteRule :: ApegRule 
whiteRule = rule "whites" ["g" .:: tLang] [] whts

                           
runMS :: FilePath -> IO ()
runMS =  runFile (runGrammar microSugar []) 
              
acceptMS :: String -> IO ()
acceptMS = runFile (runAccept  microSugar [])

debugMS :: String -> IO ()
debugMS = runFile (debugRun  microSugar [])
