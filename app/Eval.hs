module Eval where

import Control.Monad.Except ( MonadError(throwError) )

import LispVal
import LispError ( ThrowsError, LispError(BadSpecialForm, NotFunction) )
import LispPrimitive ( primitives )

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)                           = return val
eval val@(Atom _)                             = return val
eval val@(Number _)                           = return val
eval val@(Bool _)                             = return val
eval val@(Character _)                        = return val
eval (List [Atom "quote", val])               = return val
eval (List [Atom "backquote", List vals])     = List <$> mapM evalBackquote vals
     where evalBackquote (List [Atom "unquote", val]) = eval val
           evalBackquote val                          = return $ List [Atom "quote", val]

{- (DONE)
Instead of treating any non-false value as true, change the definition 
of if so that the predicate accepts only Bool values and throws an error 
on any others.
-}
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of
             Bool True  -> eval conseq
             Bool False -> eval alt
             badForm    -> throwError $ BadSpecialForm "Unrecognized special form" badForm

eval (List (Atom "cond" : clauses)) = evalCond clauses
     where evalCond [List [pred@(Bool _), action]]         = eval action
           evalCond [List [Atom "else", action]]           = eval action
           evalCond (List [pred, Atom "=>", action] : cs)  = evalCond (List [pred, action] : cs)
           evalCond (List [pred, action] : cs)             = do result <- eval pred
                                                                case result of Bool True -> eval action
                                                                               Bool False -> evalCond cs
                                                                               badForm    -> throwError $ BadSpecialForm "Unrecognized special form" badForm 
           evalCond [badForm]                              = throwError $ BadSpecialForm "Unrecognized special form" badForm 

eval (List (Atom func : args))             = mapM eval args >>= apply func
eval badForm                               = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)
