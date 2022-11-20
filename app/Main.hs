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

-- Auxiliary function to parse escape characters
parseEscape :: Parser String
parseEscape =  do
                backslash <- char '\\'
                escape <- oneOf "nrt\\\""
                let value = [backslash] ++ [escape]
                return value

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many ((l) <|> parseEscape)
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
    I know this is kind of dumb but I just wanted to make sure
    I understood how monads work at a syntactic and semantic level :p
-}

parseNumber :: Parser LispVal
parseNumber = do
                matched <- many1 digit
                (return matched) >>= (return . Number . read)
                

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
