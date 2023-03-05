module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import Numeric

import Lisp.Val
import Lisp.Error

import Util.Flow

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

{-
Excercise 2.3.5 (DONE)
Add a Character constructor to LispVal, and create
a parser for character literals as described in R5RS.
-}
parseLiteral :: Parser LispVal
parseLiteral =
    do
        char '\\'
        literal <- (string "space" >> return ' ')
               <|> (string "newline" >> return '\n')
               <|> letter
               <|> digit
               <|> symbol
        return $ Character literal

{-
Excercise 2.3.2 (DONE)
Our strings aren't quite R5RS compliant, because they don't support
escaping of internal quotes within the string. Change parseString
so that \" gives a literal quote character instead of terminating
the string. You may want to replace noneOf "\"" with a new parser 
action that accepts either a non-quote character or a backslash
followed by a quote mark.
-}

-- How the fuck do we make this work on Windows
parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many (parseEscape <|> noneOf "\"")
        char '"'
        return $ String x

{-
Excercise 2.3.3 (DONE)
Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters
-}
parseEscape :: Parser Char
parseEscape =
    do
        char '\\'
        escape <- oneOf "nrt\"\\"
        return (case escape of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    '"' -> escape
                    '\\' -> escape)

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

{-
Excercise 2.3.1 (DONE):
Rewrite parseNumber, without liftM, using
    1. do-notation (check out parseExpr)
    2. explicit sequencing with the >>= operator

Excercise 2.3.4 (DONE):
Change parseNumber to support the Scheme standard for different
bases. You may find the readOct and readHex functions useful.
-}

parseNumber :: Char -> Parser LispVal
parseNumber base =
    do
        digits <- many1 digit
        let transformer = case base of
                                'b' -> readBin
                                'o' -> readOct
                                'd' -> readDec
                                'x' -> readHex
        return $ (Number . fst . head . transformer) digits

parseQuoted :: Parser LispVal
parseQuoted =
    do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted =
    do
        char ','
        x <- parseExpr
        return $ List [Atom "unquote", x]

{-
Excercise 2.4.1 (DONE):
Add support for the backquotesyntactic sugar: the
Scheme standard details what it should expand into
(quasiquote/unquote).
-}

parseBackquoted :: Parser LispVal
parseBackquoted =
    do
        string "`("
        xs <- parseList (parseUnquoted <|> parseExpr)
        char ')'
        return $ List [Atom "backquote", xs]

{-
Excercise 2.4.3: (PENDING)
Instead of using the try combinator, left-factor the grammar
so that the common subsequence is its own parser. You should
end up with a parser that matches a string of expressions, and
one that matches either nothing or a dot and a single expression.
Combining the return values of these into either a List or a
DottedList is left as a (somewhat tricky) exercise for the
reader: you may want to break it out into another helper function.
-}

-- GOAL: Make this function as general as possible

parseList :: Parser LispVal -> Parser LispVal
parseList parseElem = List <$> sepBy parseElem spaces

parseDottedList :: Parser LispVal -> Parser LispVal
parseDottedList parseElem =
    do
        head <- endBy parseElem spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseAtom
        <|> parseNumber 'd'
        <|> (char '#' >> ((oneOf "bodx" >>= parseNumber) <|> parseLiteral))
        <|> parseQuoted
        <|> parseBackquoted
        <|> do
                char '('
                xs <- try (parseList parseExpr) <|> parseDottedList parseExpr
                char ')'
                return xs

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces) 