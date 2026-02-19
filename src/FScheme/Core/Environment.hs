module FScheme.Core.Environment where

import Control.Monad.Except
import Data.IORef
import Data.Maybe
import FScheme.Core.Error
import FScheme.Core.Types
import FScheme.Primitive.Functions
import Util.Flow

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

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

makeMacro :: [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeMacro specs corpus = return $ Macro (map showVal specs) corpus

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
