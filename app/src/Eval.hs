module Eval where

import Control.Monad.Except
import Env
import Lisp.Error
import Lisp.Primitive
import Lisp.Val
import Parser
import Util.Flow

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Character _) = return val
eval env (Atom name) = getVar env name
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "backquote", List vals]) = List <$> mapM (evalBackquote env) vals
  where
    evalBackquote domain (List [Atom "unquote", val]) = eval domain val
    evalBackquote domain val = eval domain $ List [Atom "quote", val]

{- (DONE)
Instead of treating any non-false value as true, change the definition
of if so that the predicate accepts only Bool values and throws an error
on any others.
-}
eval env (List [Atom "if", prop, conseq, alt]) =
  do
    result <- eval env prop
    case result of
      Bool True -> eval env conseq
      Bool False -> eval env alt
      badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

{-
    evalCond env (List [pred@(Bool _), action]) = eval env action
    evalCond env (List [Atom "else", action]) = eval env action
    evalCond env (List [pred, Atom "=>", action] : cs) = evalCond env (List [pred, action] : cs)
    evalCond env (List [pred, action] : cs) = do
      result <- eval env pred
      case result of
        Bool True -> eval env action
        Bool False -> evalCond env cs
        badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm
    evalCond env [badForm] = throwError $ BadSpecialForm "Unrecognized special form" badForm
-}
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom var : specs) : corpus)) =
  makeNormalFunc env specs corpus >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : specs) vargs : corpus)) =
  makeVarArgs vargs env specs corpus >>= defineVar env var
eval env (List (Atom "lambda" : List specs : corpus)) =
  makeNormalFunc env specs corpus
eval env (List (Atom "lambda" : DottedList specs vargs : corpus)) =
  makeVarArgs vargs env specs corpus
eval env (List (Atom "lambda" : vargs@(Atom _) : corpus)) =
  makeVarArgs vargs env [] corpus
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap List . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn