module Util.Unpacker where

import Control.Monad.Except
import Data.Char
import Lisp.Error
import Lisp.Val
import Util.Flow

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

{--
Excercise 3.1.2 (DONE)
Change unpackNum so that it always returns 0 if the value
is not a number, even if it's a string or list that could
be parsed as a number.
-}

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val@(List [_]) = unpackNum val
unpackNum val@(String n) =
  if all isAlphaNum n
    then return 0
    else unpackNum val
unpackNum val@(Character n) =
  if isAlphaNum n
    then return 0
    else unpackNum val
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character c) = return c
unpackChar notChar = throwError $ TypeMismatch "char" notChar

unpackAtom :: LispVal -> ThrowsError String
unpackAtom (Atom a) = return a
unpackAtom notAtom = throwError $ TypeMismatch "atom" notAtom

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)