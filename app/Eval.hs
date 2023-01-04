module Eval where

import LispVal
import LispError
import LispPrimitives (apply)
import Control.Monad
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)                        = return val
eval val@(Atom _)                          = return val
eval val@(Number _)                        = return val
eval val@(Bool _)                          = return val
eval val@(Character _)                     = return val
eval (List [Atom "quote", val])            = return val
eval (List [Atom "backquote", List vals])  = 
     do
          xs <- List <$> mapM eval vals
          return $ List [Atom "quote", xs]

{- (DONE)
Instead of treating any non-false value as true, change the definition 
of if so that the predicate accepts only Bool values and throws an error 
on any others.
-}
eval (List [Atom "if", pred@(Bool _), conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool True  -> eval conseq
             Bool False -> eval alt

eval (List [Atom "cond", ]) = 
eval (List (Atom func : args))             = mapM eval args >>= apply func
eval badForm                               = throwError $ BadSpecialForm "Unrecognized special form" badForm
