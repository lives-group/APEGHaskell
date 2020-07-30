module APEG.PlayGround where

import APEG.TypeSystem
import APEG.Interpreter.DT
import APEG.Interpreter.APEGInterp
import APEG.Interpreter.Value
import APEG.Interpreter.State
import APEG.Interpreter.MaybeString
import APEG.AbstractSyntax
import Control.Monad.State.Lazy




runGrammar :: ApegGrm -> [(Var,Value)] -> String -> IO ()
runGrammar g vs s = do (st, ps) <- return  (runState (interpGrammar vs g) (zeroSt g s))
                       case (getResult ps) of
                            (Right dt) ->  putStrLn (pprintDT $ head dt) >> putStrLn "ACCEPTED" 
                            (Left xs)  ->  putStrLn "REJECTED ! Error list:" >> 
                                           putStrLn (unlines xs) >> 
                                           putStrLn "-----------------------------" >>
                                           putStrLn "Remaining input:" >>
                                           putStrLn (roundRemInp 20 ps)

                              
debugRun :: ApegGrm -> [(Var,Value)] -> String -> IO ()
debugRun g vs s = do (st,ps) <- return (runState (interpGrammar vs g) (zeroSt g s))
                     putStrLn " ============= Type Context ================" 
                     putStrLn (pprintTyEnv (tyEnv ps))
                     putStrLn " ========= End of type Context ============="
                     case (getResult ps) of
                            (Right dt) ->  putStrLn "ACCEPTED"                             
                            (Left xs)  ->  putStrLn "REJECTED !" >> putStrLn (unlines xs)
                                    

simpleRun :: ApegGrm -> [(Var,Value)] -> String -> IO ()
simpleRun g vs s = do (st,ps) <- return (runState (interpGrammar vs g) (zeroSt g s))
                      case (getResult ps) of
                            (Right dt) ->  putStrLn "ACCEPTED" 
                            (Left xs)  ->  putStrLn "REJECTED ! Erros:" >> 
                                           putStrLn (unlines xs) >> 
                                           putStrLn "-----------------------------" >>
                                           putStrLn "Remaining input:" >>
                                           putStrLn (roundRemInp 20 ps)

                            
runAccept ::  ApegGrm -> [(Var,Value)] -> String -> IO ()
runAccept g vs s =  putStrLn (show $ acceptTest g vs s)

acceptTest :: ApegGrm -> [(Var,Value)] -> String -> Bool
acceptTest g vs s = sel $ runState (interpGrammar vs g) (zeroSt g s)
    where sel (_,st) = case getResult st of
                         (Right _) -> True
                         (Left _) -> False

runFile ::  (String -> IO ()) -> FilePath -> IO ()
runFile p fs = do s <- readFile fs
                  p s
