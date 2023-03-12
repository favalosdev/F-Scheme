module Util.Flow where

import Control.Monad.Except
import Lisp.Error

type ThrowsError = Either LispError

{-
  If an error is caught, it is converted as a string
  so that it can be handled more easily.
-}
trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = action `catchError` (return . show)

{-
  Unwraps a value from the ThrowsError constructor.
  Left undefined for a left constructor because
  when this function is invoked, error handling
  has already been performed.
-}
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ExceptT LispError IO

-- Recontextualises erros in different types of monads.
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

{-
  Mental model:
  (1) Trap errors and convert them into strings if necessary.
      Result should be of type IO (ThrowsError String)
  (2) Apply (quite literally) the extractValue function and
      get the final result.
-}
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)