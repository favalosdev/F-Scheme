module Eval where

import Control.Monad.Except ( MonadError(throwError) )

import LispVal
import LispError ( ThrowsError, LispError(BadSpecialForm, NotFunction) )
import LispPrimitive ( primitives )
import Env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _)                             = return val
eval _ val@(Atom _)                               = return val
eval _ val@(Number _)                             = return val
eval _ val@(Bool _)                               = return val
eval _ val@(Character _)                          = return val
eval _ (List [Atom "quote", val])                 = return val
eval env (List [Atom "backquote", List vals])     = List <$> mapM (evalBackquote env) vals
     where evalBackquote env (List [Atom "unquote", val]) = eval env val
           evalBackquote env val                          = return $ List [Atom "quote", val]

{- (DONE)
Instead of treating any non-false value as true, change the definition 
of if so that the predicate accepts only Bool values and throws an error 
on any others.
-}
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool True  -> eval env conseq
             Bool False -> eval env alt
             badForm    -> throwError $ BadSpecialForm "Unrecognized special form" badForm

eval env (List (Atom "cond" : clauses)) = evalCond env clauses
     where evalCond env [List [pred@(Bool _), action]]         = eval env action
           evalCond env [List [Atom "else", action]]           = eval env action
           evalCond env (List [pred, Atom "=>", action] : cs)  = evalCond env (List [pred, action] : cs)
           evalCond env (List [pred, action] : cs)             = do result <- eval env pred
                                                                    case result of Bool True -> eval env action
                                                                                   Bool False -> evalCond env cs
                                                                                   badForm    -> throwError $ BadSpecialForm "Unrecognized special form" badForm 
           evalCond env [badForm]                              = throwError $ BadSpecialForm "Unrecognized special form" badForm 

eval env (List (Atom func : args))             = mapM (eval env) args >>= liftThrows . apply func
eval _ badForm                               = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)
