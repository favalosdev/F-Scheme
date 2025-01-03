module Main where

import System.Environment
import FScheme.REPL.Console

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args