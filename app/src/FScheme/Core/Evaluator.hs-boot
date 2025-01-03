module FScheme.Core.Evaluator where

import FScheme.Core.Types
import Util.Flow
import {-# SOURCE #-} FScheme.Core.Environment

eval :: Env -> LispVal -> IOThrowsError LispVal
