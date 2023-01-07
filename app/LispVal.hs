{-# LANGUAGE InstanceSigs #-}
module LispVal where

import Env

data LispVal = Atom String
            |  List [LispVal]
            |  DottedList [LispVal] LispVal
            |  Number Integer
            |  String String
            |  Bool Bool
            |  Character Char
            |  Float Rational
            |  PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            |  Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: Env }

type ThrowsError = Either LispError

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (Character literal)    = [literal]
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showval (PrimitiveFunc _)      = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show :: LispVal -> String
                            show = showVal
