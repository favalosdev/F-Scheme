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
parseNumber = parseQuantity 'd' <|> (char '#' >> (oneOf "bodx" >>= parseQuantity))
    
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList =
    do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail

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
