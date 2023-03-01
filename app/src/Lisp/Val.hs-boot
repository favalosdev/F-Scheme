module Lisp.Val where

data LispVal

unwordsList :: [LispVal] -> String 
showVal :: LispVal -> String

instance Show LispVal 