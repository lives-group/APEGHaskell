module LangSample where

import APEGInterp
import Control.Monad.State.Lazy
import AbstractSyntax
import APEGState
import APEGTypeSystem

alts :: [APeg] -> APeg
alts  = foldr1 Alt

seqs :: [APeg] -> APeg
seqs = foldr1 Seq 

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

runGrmEx1 ::   String -> SmallTuple 
runGrmEx1 s = simpleTestWithArgs ex2 [] s

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
                       
--  ================= EXAMPLE 3 =================
-- Using Bind : 
-- 
-- S[Lan g] returns [String out] -> out = NUM 
--                                / ou = VAR
-- NUM[Lan g] -> (0 \ 1) (0 \ 1)*
-- VAR[Lan g] -> ('A' \ 'B') ('A' \ 'B')*

runGrmEx3 ::   String -> SmallTuple 
runGrmEx3 s = simpleTestWithArgs ex3 [] s

ex3 :: ApegGrm
ex3 = [r1ex3,r2ex3,r3ex3]

r1ex3 :: ApegRule
r1ex3 = ApegRule "S" [(TyLanguage, "g")] [(TyStr,EVar "out")]  ( Alt (Bind "out" (NT "VAR" [EVar "g"] [])) 
                                                               (Bind "out" (NT "NUM" [EVar "g"] [])) )

r2ex3 :: ApegRule
r2ex3 = ApegRule "VAR" [(TyLanguage, "g")] [] (let x = (Alt (Lit "A") (Lit "B")) in Seq x (Kle x))


r3ex3 :: ApegRule
r3ex3 = ApegRule "NUM" [(TyLanguage, "g")] [] (let x = (Alt (Lit "0") (Lit "1")) in Seq x (Kle x))



