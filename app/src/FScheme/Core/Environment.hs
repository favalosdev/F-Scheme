module FScheme.Core.Environment where

import Control.Monad.Except
import Data.IORef
import Data.Maybe
import FScheme.Core.Error
import FScheme.Primitive.Functions
import FScheme.Core.Types
import Control.Monad.IO.Class
import Util.Flow

type Env = IORef [(String, IORef LispVal)]

{-
  Cheat-sheet:
  1. liftIO takes an IO wrapped value and "recontextualizes" it into
    another MonadIO.
  2. newIORef takes a value, constructs an IORef and wraps it in the
    IO monad.
  3. readIORef takes a value passed to an IORef and wraps it in the IO
    monad.
  4. writeIORef takes an IORef, a value and presumably overwrites it.
    An empty type IO monad is returned - I presume - because of convenience.

  All the functions in here build from these first principles.
-}

-- Creates a "null" env i.e. an empty tuple list
nullEnv :: IO Env
nullEnv = newIORef []

-- Self-explanatory
isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

-- Self-explanatory
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (`writeIORef` value))
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef pairs = readIORef envRef >>= extendEnv pairs >>= newIORef
  where
    extendEnv ps env = (++ env) <$> mapM addBinding ps 
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc vargs env specs corpus = return $ Func (map showVal specs) vargs corpus env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= flip
      bindVars
      ( map (makeFuncAux IOFunc) ioPrimitives
          ++ map (makeFuncAux PrimitiveFunc) primitives
      )
  where
    makeFuncAux constructor (var, func) = (var, constructor func)
