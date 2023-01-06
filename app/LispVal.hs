module LispVal where

import LispError

data LispVal = Atom String
            |  List [LispVal]
            |  DottedList [LispVal] LispVal
            |  Number Integer
            |  String String
            |  Bool Bool
            |  Character Char
            |  Float Rational

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (Character literal)    = [literal]
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

type ThrowsError = Either LispError

{--
Excercise 3.1.2 (DONE)
Change unpackNum so that it always returns 0 if the value
is not a number, even if it's a string or list that could
be parsed as a number.
-}

-- Pending authorization of unpacking numbers
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n)     = return n
unpackNum val@(List [n]) = unpackNum val
unpackNum val@(String n) = if and $ map isAlphaNum n
                           then return 0
                           else unpackNum val 

unpackNum val@(Character n) = if isAlphaNum n
                              then return $ 0
                              else unpackNum val 

unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character c) = return c
unpackChar notChar       = throwError $ TypeMismatch "char" notChar

unpackAtom :: LispVal -> ThrowsError String
unpackAtom (Atom a) = return a
unpackAtom notAtom  = throwError $ TypeMismatch "atom" notAtom

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)