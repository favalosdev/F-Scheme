module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
            |  List [LispVal]
            |  DottedList [LispVal] LispVal
            |  Number Integer
            |  String String
            |  Bool Bool
            |  Character Char

-- Task: Parse literals accordng to R5RS standard

parseSpaceLiteral :: Parser Char
parseSpaceLiteral = do
    string "space"
    return ' '

parseNewlineLiteral :: Parser Char
parseNewlineLiteral = do
    string "newline"
    return '\n'

parseLiteral :: Parser LispVal
parseLiteral = do
    char '#'
    char '\\'
    literal <- (anyChar <|> parseSpaceLiteral <|> parseNewlineLiteral)
    return $ Character literal

-- Auxiliary function for parsing escape characters

parseEscape :: Parser Char
parseEscape = do
    char '\\'
    escape <- oneOf "nrt\"\\"
    return (case escape of
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                '"' -> '"'
                '\\' -> '\\')

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ parseEscape <|> anyChar
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom


-- Task: Rewrite without liftM but with do-notation and >>= binding

{-
    I know this is kind of dumb but I just wanted to make sure I
    really understood how monads work at a syntactic and semantic
    level :p
-}

-- Pending: implementation of binary to decimal converter

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
