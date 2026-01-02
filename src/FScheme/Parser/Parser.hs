module FScheme.Parser.Parser where

import Control.Monad.Except
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

import FScheme.Core.Error
import FScheme.Core.Types

import Util.Flow

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseLiteral :: Parser LispVal
parseLiteral =
  do
    _ <- char '\\'
    literal <-
      (string "space" >> return ' ')
        <|> (string "newline" >> return '\n')
        <|> letter
        <|> digit
        <|> symbol
    return $ Character literal

parseString :: Parser LispVal
parseString =
  do
    _ <- char '"'
    x <- many (parseEscape <|> noneOf "\"")
    _ <- char '"'
    return $ String x

parseEscape :: Parser Char
parseEscape =
  do
    _ <- char '\\'
    escape <- oneOf "nrt\"\\"
    return $ case escape of
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          '"' -> escape
          '\\' -> escape

parseAtom :: Parser LispVal
parseAtom =
  do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom (first : rest)

parseBool :: Parser LispVal
parseBool =
  do
    base <- oneOf "tf"
    return $ case base of
      't' -> Bool True
      'f' -> Bool False

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
  
parseNonDecNumber :: Parser LispVal
parseNonDecNumber = oneOf "bodx" >>= parseNumber

parseQuoted :: Parser LispVal
parseQuoted =
  do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted =
  do
    _ <- char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseBackquoted :: Parser LispVal
parseBackquoted =
  do
    _ <- string "`("
    xs <- parseList parseExpr
    _ <- char ')'
    return $ List [Atom "backquote", xs]

parseList :: Parser LispVal -> Parser LispVal
parseList parseElem = List <$> sepBy parseElem spaces

parseDottedList :: Parser LispVal -> Parser LispVal
parseDottedList parseElem =
  do
    x <- endBy parseElem spaces
    xs <- char '.' >> spaces >> parseExpr
    return $ DottedList x xs

parseExpr :: Parser LispVal
parseExpr =
    parseNumber 'd'
    <|> (char '#' >> (parseNonDecNumber <|> parseBool <|> parseLiteral))
    <|> parseAtom
    <|> parseString
    <|> parseQuoted
    <|> parseBackquoted
    <|> parseUnquoted
    <|> do
      _ <- char '('
      xs <- try (parseList parseExpr) <|> parseDottedList parseExpr
      _ <- char ')'
      return xs

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "Scheme" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
