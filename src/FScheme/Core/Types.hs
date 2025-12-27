{-# LANGUAGE InstanceSigs #-}

module FScheme.Core.Types where

import {-# SOURCE #-} FScheme.Core.Environment
import System.IO
import Util.Flow

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Rational
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String],
        varargs :: Maybe String,
        body :: [LispVal],
        closure :: Env
      }
  | Macro
      { params :: [String],
        body :: [LispVal],
        closure :: Env
      }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float x) = show x
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character literal) = [literal]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ showVal xs ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Func {params = args, varargs = vargs, body = _, closure = _}) =
  "(lambda ("
    ++ unwords (map show args)
    ++ ( case vargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"
showVal (Macro {params = args, body = _ }) =
  "(lambda ("
    ++ unwords (map show args)
    ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show :: LispVal -> String
  show = showVal
