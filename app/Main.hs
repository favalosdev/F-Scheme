module Main where

-- Commit test

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

instance Show LispVal where show = showVal

-- Parsing section

{-
Excercise 2.3.5 (PENDING FOR TESTING)
Add a Character constructor to LispVal, and create
a parser for character literals as described in R5RS.
-}

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseLiteral :: Parser LispVal
parseLiteral =
    do
        char '#'
        char '\\'
        literal <- (anyChar <|> parseSpaceLiteral <|> parseNewlineLiteral)
        return $ Character literal

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


{-
Excercise 2.3.2 (PENDING FOR TESTS)
Our strings aren't quite R5RS compliant, because they don't support
escaping of internal quotes within the string. Change parseString
so that \" gives a literal quote character instead of terminating
the string. You may want to replace noneOf "\"" with a new parser 
action that accepts either a non-quote character or a backslash
followed by a quote mark.
-}
parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many (parseEscape <|> anyChar)
        char '"'
        return $ String x

{-
Excercise 2.3.3 (PENDING FOR TESTS)
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
    1. do-notation
    2. explicit sequencing with the >>= operator

Excercise 2.3.4 (DONE):
Change parseNumber to support the Scheme standard for different
bases. You may find the readOct and readHex functions useful.
-}

parseNumber :: Parser LispVal
parseNumber = parseQuantity 'd'
          <|> do
                char '#'
                oneOf "bodx" >>= parseQuantity

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

{-
Excercise 2.4.1:
Add support for the backquotesyntactic sugar: the
Scheme standard details what it should expand into
(quasiquote/unquote).
-}

parseQuoted :: Parser LispVal
parseQuoted =
    do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]


{-
Excercise 2.4.3: (PENDING FOR TEST)
Instead of using the try combinator, left-factor the grammar
so that the common subsequence is its own parser. You should
end up with a parser that matches a string of expressions, and
one that matches either nothing or a dot and a single expression.
Combining the return values of these into either a List or a
DottedList is left as a (somewhat tricky) exercise for the
reader: you may want to break it out into another helper function.
-}

-- Make this function as general as possible
parseList :: Parser LispVal
parseList = 
    do
        common <- parseCommon
        rest <- parseNothing <|> parseDottedRest
        return $ case rest of
            List _ -> List common
            _ -> DottedList common rest

-- This is rather unconventional but I'm running out of ideas
parseNothing :: Parser LispVal
parseNothing =
    do
        string []
        return $ List []

parseCommon :: Parser [LispVal]
parseCommon = sepBy parseExpr spaces

parseDottedRest :: Parser LispVal
parseDottedRest = spaces >> char '.' >> spaces >> parseExpr

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do 
                char '('
                x <- parseList
                char ')'
                return x

-- Evaluation: part 1

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

-- Output

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
