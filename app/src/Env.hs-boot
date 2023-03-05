module Env where
import Data.IORef
import {-# SOURCE #-} Lisp.Val

type Env = IORef [(String, IORef LispVal)]

bindVars :: Env -> [(String, LispVal)] -> IO Env