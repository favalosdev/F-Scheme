module FScheme.Core.Types where

data LispVal

unwordsList :: [LispVal] -> String
showVal :: LispVal -> String

instance Show LispVal