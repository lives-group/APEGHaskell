 module APEG.ASTSamples.LangSample where

import APEG.Interpreter.APEGInterp
import APEG.Interpreter.MonadicState
import APEG.Interpreter.State
import APEG.Interpreter.MaybeString
import Control.Monad.State.Lazy
import APEG.AbstractSyntax
-- import APEGState
import APEG.TypeSystem


alts :: [APeg] -> APeg
alts  = foldl1 Alt

seqs :: [APeg] -> APeg
seqs = foldl1 Seq

chrs :: [Char] -> APeg
chrs = alts.(map (Lit.(:[])))

many1 :: APeg -> APeg
many1 p = Seq p (Kle p) 

g :: Expr
g = EVar "g"


-- ================= EXAMPLE 1 =================
-- Testing simple PEG and APEG expressions !


runEx1 :: String -> ((),PureState) 
runEx1 s = runState (interp r1ex1)  (zeroSt [] s)

runEx2 :: String -> ((),PureState) 
runEx2 s = runState (interp r2ex1)  (zeroSt [] s)

runEx3 :: String -> ((),PureState) 
runEx3 s = runState (interp r3ex1)  (zeroSt [] s)

runEx4 :: String -> ((),PureState) 
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
r4ex1 = Alt (Seq (Update [("y",Str "Yka")] )
                 (Seq (Not (Lit "ad")) 
                      (Seq (Lit "b")
                           (Seq (Lit "c")
                                (Update [("x",Str "ba")]) ))))   
            (Seq (Update [("y",Str "nah")])
                 (Seq (Lit "010")
                      (Seq (Lit "101")
                           (Update [("x",Str "neh")] ))))
                           
--  ================= EXAMPLE 2 =================
-- Creating a Simple Grammar: 
-- 
-- S[Lan g] -> 'a' S<g> 'b'
--           / '.'

runGrmEx1 ::   String -> (VEnv,MybStr,String,Result) 
runGrmEx1 s = simpleTestWithArgs ex2 [] s

ex2 :: ApegGrm
ex2 = [r1ex2]
-- S<g> : 'a'S<g> 'b' / '.';

r1ex2 :: ApegRule
r1ex2 = ApegRule "S" 
                 [(TyLanguage,"g")] 
                 [] 
                 ( Alt (Seq (Lit "a")
                            (Seq (NT "S" [EVar "g"] [])
                                 (Lit "b")))
                       (Lit ".") )
                       
--  ================= EXAMPLE 3 =================
-- Using Bind : 
-- 
-- S[Lan g] returns [out] -> out = VAR 
--                         / out = NUM
-- NUM[Lan g] -> (0 \ 1) (0 \ 1)*
-- VAR[Lan g] -> ('A' \ 'B') ('A' \ 'B')*

runGrmEx3 ::   String -> (VEnv,MybStr,String,Result) 
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


-- ===============


meta01 = MkSeq (MetaPeg $ MkLit (Str "o")) (MetaPeg $ MkKle $ MetaPeg (MkAlt (MetaPeg $ MkLit (Str "0")) (MetaPeg $ MkLit (Str "1"))))
metaAB = MkKle $ MetaPeg (MkAlt (MetaPeg $ MkLit (Str "A")) (MetaPeg $ MkLit (Str "B")))

mr1 = MkRule (MetaExp $ MStr (Str "fator")) [(MetaExp MkTyLanguage,MetaExp $ MVar (Str "g"))] [] (MetaPeg meta01)

grmExtension :: ApegGrm
grmExtension = [ruleStart, ruleExt, ruleFator] 



ruleStart :: ApegRule 
ruleStart = ApegRule "start"
                     [(TyLanguage,"g")]
                     []
                     (seqs [NT "ext" [EVar "g"] ["sigma"],
                            NT "fator" [Union (EVar "g") (EVar "sigma")] [] ] )

ruleExt :: ApegRule 
ruleExt = ApegRule "ext"
                    [(TyLanguage,"g")]
                    [(TyLanguage, EVar "si")]
                    (alts [seqs [Lit ".", Update [("si",mr1)]],
                           seqs [Lit ",", Update [("si",MkRule (MetaExp $ MStr (Str "fator")) 
                                                               [(MetaExp MkTyLanguage,MetaExp $ MVar (Str "g"))] 
                                                               [] 
                                                               (MetaPeg metaAB)) ]],
                           seqs [Lambda, Update [("si",Epsilon)]]
                           ])
                    
ruleFator :: ApegRule 
ruleFator = ApegRule "fator"
                     [(TyLanguage,"g")]
                     []
                     (Kle $ alts [ Lit "x", Lit "y"  ])
