module APEG.Combinators where

import APEG.AbstractSyntax
import APEG.Interpreter.MonadicState
import APEG.Interpreter.State
import APEG.Interpreter.Value
import APEG.Interpreter.DT
import qualified Data.Map as M
import Control.Monad.State.Lazy





implicitLanArg :: [Value] -> APegSt ([Value])
implicitLanArg [] = getLanguage >>= return.(:[]).vlan
implicitLanArg xs@((VLan _):_) = return xs
implicitLanArg (xs) = getLanguage >>= return.(:xs).vlan

onSucess :: APegSt a -> APegSt a -> APegSt a
onSucess suc fai = isOk >>= f
    where f True  = suc
          f False = fai

try :: APegSt () -> APegSt ()
try p = do s' <- get
           r <- p
           onSucess (return ()) (put s' >> pfail)

klenne :: APegSt a -> APegSt [a]
klenne p =  
     do s <- get 
        x <- p 
        xs <- onSucess (klenne p) (put s >> done >> return [])
        return (x:xs)            

sequential :: APegSt () -> APegSt () -> APegSt ()
sequential p q = do p 
                    r <- isOk
                    if r then q else pfail
                    
alternate :: APegSt () -> APegSt () -> APegSt ()
alternate l r = do s <- get 
                   l
                   onSucess (return ()) (put s >> r) 
                   
notPeg :: APegSt () -> APegSt ()
notPeg p = do pst <- get
              -- (_,_,_,reckon,inp,_) <- get 
              p
              -- (grm,env,tyEnv,_,_,res) <- get 
              pst'<- get
              let pst'' = ((setInput (remInp pst')).(setPrefix (getPrefix pst))) pst' 
                in onSucess (put pst'' >> pfail) (put pst'' >> done) 
              



