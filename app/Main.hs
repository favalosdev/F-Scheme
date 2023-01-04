module Main where

import System.Environment
import Control.Monad
import Control.Monad.Except
import LispParser (readExpr)
import Eval (eval)
import LispError

main :: IO ()
main = do 
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
