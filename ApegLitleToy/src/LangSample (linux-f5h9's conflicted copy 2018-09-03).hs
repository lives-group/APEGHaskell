module LangSample where

import APEGInterp
import Control.Monad.State.Lazy
import AbstractSyntax



-- ================= EXAMPLE 1 =================
-- Testing simple PEG and APEG expressions !


runEx1 :: String -> ((),ApegTuple) 
runEx1 s = runState (interp r1ex1)  (zeroSt [] s)

runEx2 :: String -> ((),ApegTuple) 
runEx2 s = runState (interp r2ex1)  (zeroSt [] s)

runEx3 :: String -> ((),ApegTuple) 
runEx3 s = runState (interp r3ex1)  (zeroSt [] s)

runEx4 :: String -> ((),ApegTuple) 
runEx4 s = runState (interp r4ex1)  (zeroSt [] s)
-- ac*a
r1ex1 :: APeg
r1ex1 = Seq (Lit "a")      
            (Seq (Kle (Lit "c")) (Lit "a"))
 
-- a / c*b 
r2ex1 :: APeg
r2ex1 = Alt (Lit "a")    
            (Seq (Kle (Lit "c")) (Lit "b"))

-- !'ad' b c 
r3ex1 :: APeg
r3ex1 = Seq (Not (Lit "ad"))   
            (Seq (Kle (Lit "c")) (Lit "b"))

            --  {y = "Yka";} !'ad'  b c { x = "ba"; } / { y = "nah"; } '010' '101' { x = "neh"; }
r4ex1 :: APeg
r4ex1 = Alt (Seq (AEAttr [("y",Str "Yka")] )
                 (Seq (Not (Lit "ad")) 
                      (Seq (Lit "b")
                           (Seq (Lit "c")
                                (AEAttr [("x",Str "ba")]) ))))   
            (Seq (AEAttr [("y",Str "nah")])
                 (Seq (Lit "010")
                      (Seq (Lit "101")
                           (AEAttr [("x",Str "neh")] ))))
                           
--  ================= EXAMPLE 2 =================
-- Creating a Simple Grammar: 
-- 
-- S[Lan g] -> 'a' S<g> 'b'
--           / '.'

runGrmEx1 ::   String -> ((),ApegTuple) 
runGrmEx1 s = runState (interpGrammar [] ex2) (zeroSt ex2 s)

ex2 :: ApegGrm
ex2 = [r1ex2]

r1ex2 :: ApegRule
r1ex2 = ApegRule "S" 
                 [(TyLanguage,"g")] 
                 [] 
                 ( Alt (Seq (Lit "a")
                            (Seq (NT "S" [] [])
                                 (Lit "b")))
                       (Lit ".") )



