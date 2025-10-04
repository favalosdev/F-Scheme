module FScheme.Core.Environment where
import Data.IORef
import {-# SOURCE #-} FScheme.Core.Types

type Env = IORef [(String, IORef LispVal)]

bindVars :: Env -> [(String, LispVal)] -> IO Env