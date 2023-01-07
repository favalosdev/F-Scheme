module Main where

import System.Environment ( getArgs )
import Repl (runOne, runRepl)

main :: IO ()
main = do args <- getArgs
          case length args of
                0 -> runRepl
                1 -> runOne $ head args
                _ -> putStrLn "Progam takes only 0 or 1 argument"
