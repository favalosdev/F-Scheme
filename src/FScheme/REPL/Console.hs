module FScheme.REPL.Console where

import FScheme.Core.Environment
import FScheme.Core.Evaluator
import FScheme.Core.Types
import System.IO
import Util.Flow

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ prop prompt action = do
  result <- prompt
  if prop result 
    then return ()
    else action result >> until_ prop prompt action

runOne :: [String] -> IO ()
runOne args = do
  let filename = String $ head args
  let largs = List $ map String $ drop 1 args
  env <- primitiveBindings >>= flip bindVars [("args", largs)]
  runIOThrows (show <$> eval env (List [Atom "load", filename]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "F-Scheme>>> ") . evalAndPrint
