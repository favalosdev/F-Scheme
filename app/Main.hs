module Main where

import System.Environment ( getArgs )
import Repl (runOne, runRepl)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args