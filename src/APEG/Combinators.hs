module APEG.Combinators where

import APEG.AbstractSyntax
import APEG.Interpreter.MonadicState
import APEG.Interpreter.State
import APEG.Interpreter.Value
import APEG.Interpreter.MaybeString
import APEG.Interpreter.DT
import qualified Data.Map as M
import Control.Monad.State.Lazy


implicitLanArg :: [Value] -> APegSt ([Value])
implicitLanArg [] = getLanguage >>= (\(g,t) -> return [vlan t g] )
implicitLanArg xs@((VLan _ _):_) = return xs
implicitLanArg (xs) = getLanguage >>= (\(g,t) -> return [vlan t g] )

onSuccess :: APegSt a -> APegSt a -> APegSt a
onSuccess suc fai = isOk >>= f
    where f True  = suc
          f False = fai

try :: APegSt () -> APegSt ()
try p = do s' <- get
           r <- p
           onSuccess (return ()) (put s' >> pfail)

klenne :: APegSt a -> APegSt [a]
klenne p =  
     do s <- get 
        x <- p 
        xs <- onSuccess (klenne p) (put s >> done >> return [])
        return (x:xs)            

sequential :: APegSt () -> APegSt () -> APegSt ()
sequential p q = do st <- get
                    p 
                    onSuccess q (put st >> pfail)
                    
alternate :: APegSt () -> APegSt () -> APegSt ()
alternate l r = do s <- get 
                   l
                   onSuccess (return ()) (put s >> r) 
                   
notPeg :: APegSt () -> APegSt ()
notPeg p = do pst <- get
              p
              pst'<- get
              let pst'' = ((setInput (remInp pst')).(setPrefix (getPrefix pst))) pst' 
                in onSuccess (put pst'' >> pfail) (put pst'' >> done) 
                              
constraintApeg :: Value -> APegSt () -> APegSt ()
constraintApeg (VBool True)  p  = p
constraintApeg (VBool False) _  = pfail  
constraintApeg _ _ = error "Non boolean value used at the test on a condition APEG expression."

bindApeg :: Var -> APegSt () -> APegSt ()
bindApeg s p = do prfx <- getStr
                  resetStr
                  p
                  res <- getStr
                  modify (prependPrefix prfx)
                  varSet s (vstr $ fromMybStr res) 

update :: Var -> APegSt Value -> APegSt ()
update v p =  p >>= (varSet v)

