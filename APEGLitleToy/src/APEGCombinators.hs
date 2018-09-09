module APEGCombinators where

import AbstractSyntax
import APEGState
import qualified Data.Map as M

-- import Data.Either
import Control.Monad.State.Lazy
import APEGState

done :: APegSt ()
done =  modify (\(grm,env,tyEnv,reckon,inp,res) -> (grm,env,tyEnv,reckon,inp,True)) 

pfail :: APegSt ()
pfail = modify (\(grm,env,tyenv,reckon,inp,r) -> (grm,env,tyenv,reckon,inp,False))

prefix :: String -> String -> (String,String)
prefix xs ys = test (splitAt (length xs) ys)
   where
       test (p,zs)
           | p == xs = (p,zs)
           | otherwise = ([],ys)

patternMatch :: String -> APegSt ()
patternMatch s = do (grm,env,tyEnv,reckon,inp,res) <- get
                    case prefix s inp of
                         ([],_) ->  put (grm,env,tyEnv,reckon,inp,False)
                         (xs,ys) -> put (grm,env,tyEnv,reckon++xs,ys,True)

implicitLanArg :: [Value] -> APegSt ([Value])
implicitLanArg [] = language >>= return.(:[]).VLan
implicitLanArg xs@((VLan _):_) = return xs
implicitLanArg (xs) = language >>= return.(:xs).VLan

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
alternate l r = try l >> onSucess (return ()) (done >> r) 
                   
notPeg :: APegSt () -> APegSt ()
notPeg p = do (_,_,_,reckon,inp,_) <- get 
              p
              (grm,env,tyEnv,_,_,res) <- get 
              onSucess (put (grm,env,tyEnv,reckon,inp,False)) (put (grm,env,tyEnv,reckon,inp,True) >> return ()) 
              



