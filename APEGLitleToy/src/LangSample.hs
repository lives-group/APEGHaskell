module LangSample where

import APEGInterp
import Control.Monad.State.Lazy
import AbstractSyntax




-- ================= EXAMPLE 1 =================

runEx1 :: String -> ((),ApegTuple) 
runEx1 s = runState (interp r1ex1)  (zeroSt [] s)

r1ex1 :: APeg
r1ex1 = Seq (Lit "a")      --The body of the rule 
            (Seq (Kle (Lit "c")) (Lit "a"))
