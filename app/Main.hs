module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
            |  List [LispVal]
            |  DottedList [LispVal] LispVal
            |  Number Integer
            |  String String
            |  Bool Bool
            |  Character Char
            |  Float Rational

-- Task: Parse literals accordng to R5RS standard

parseSpaceLiteral :: Parser Char
parseSpaceLiteral =
    do
        string "space"
        return ' '

parseNewlineLiteral :: Parser Char
parseNewlineLiteral =
    do
        string "newline"
        return '\n'

parseLiteral :: Parser LispVal
parseLiteral =
    do
        char '#'
        char '\\'
        literal <- (anyChar <|> parseSpaceLiteral <|> parseNewlineLiteral)
        return $ Character literal

-- Auxiliary function for parsing escape characters

parseEscape :: Parser Char
parseEscape =
    do
        char '\\'
        escape <- oneOf "nrt\"\\"
        return (case escape of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    '"' -> '"'
                    '\\' -> '\\')

parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many $ parseEscape <|> anyChar
        char '"'
        return $ String x

parseAtom :: Parser LispVal
parseAtom = 
    do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom


parseQuantity :: Char -> Parser LispVal
parseQuantity base =
    do
        digits <- many1 digit
        let transformer = case base of
                                'b' -> readBin
                                'o' -> readOct
                                'd' -> readDec
                                'x' -> readHex
        return $ (Number . fst . head . transformer) digits

parseNumber :: Parser LispVal
parseNumber = parseQuantity 'd'
          <|> do
                char '#'
                oneOf "bodx" >>= parseQuantity base

{-
    Excercise:
    (1) Add support for the backquote syntactic sugar: the Scheme standard details
        what it should expand into (quasiquote/unquote).

    (2) Add support for vectors. The Haskell representation is up to you: GHC does
        have an Array data type, but it can be difficult to use. Strictly speaking, a
        vector should have constant-time indexing and updating, but destructive update
        in a purely functional language is difficult. You may have a better idea how to do
        this after the section on set!, later in this tutorial.

    (3) Instead of using the try combinator, left-factor the grammar so that the
        common subsequence is its own parser. You should end up with a parser that
        matches a string of expressions, and one that matches either nothing or a dot
        and a single expression. Combining the return values of these into either a List
        or a DottedList is left as a (somewhat tricky) exercise for the reader: you may
        want to break it out into another helper function.
-}

parseCommonSubsequence :: Parser LispVal
parseCommonSubsequence = liftM List $ sepBy parseExpr spaces

parseTail :: Parser LispVal
parseTail = eof <|> parseExpr

parseList :: Parser LispVal


{-
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList =
    do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail
-}

parseQuoted :: Parser LispVal
parseQuoted =
    do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

-- Pending: implementation of binary to decimal converter

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do 
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

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
