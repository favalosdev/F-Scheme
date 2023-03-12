module Repl where

import Control.Monad
import Env
import Eval
import Lisp.Error
import Lisp.Val
import Parser
import System.IO
import Util.Flow

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  let filename = head args
  let largs = drop 1 args
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String largs)]
  runIOThrows (show <$> eval env (List [Atom "load", String filename]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "F-Scheme>>> ") . evalAndPrint
