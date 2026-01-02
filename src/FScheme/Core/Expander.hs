module FScheme.Core.Expander where

import FScheme.Core.Environment
import FScheme.Core.Types
import Util.Flow
import Control.Monad.Except
import Data.IORef

expand :: Env -> LispVal -> IOThrowsError LispVal
expand _ (List [Atom "quote", val]) = return val
expand env (List [Atom "unquote", val]) = expand env val
expand env (List [Atom "backquote", List vals]) = List <$> mapM (expand env) vals
expand env (List vals) = List <$> mapM (expand env) vals
expand env val@(Atom name) =
  do
    e <- liftIO $ readIORef env
    maybe
      (return val)
      (liftIO . readIORef)
      (lookup name e)
expand _ val = return val