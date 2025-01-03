{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module FScheme.Primitive.Functions where

import Control.Monad.Except
import Data.Maybe
import {-# SOURCE #-} FScheme.Core.Environment
import {-# SOURCE #-} FScheme.Core.Evaluator
import FScheme.Core.Error
import FScheme.Core.Types
import FScheme.Parser.Parser
import System.IO
import Util.Flow
import Util.Unpacker

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op specs = Number . foldl1 op <$> mapM unpackNum specs

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

{-
Excercise 3.1.1 (IN CONSTRUCTION)
Add primitives to perform the various type-testing
functions of R5RS: symbol?, string?, number?, etc.
-}

isType :: Unpacker -> [LispVal] -> ThrowsError LispVal
isType (AnyUnpacker unpacker) args =
  if length args /= 1
    then throwError $ NumArgs 1 args
    else do
      return $ case unpacker $ head args of
        Right _ -> Bool True
        Left _ -> Bool False

isSymbol, isString, isNumber, isChar, isBool :: [LispVal] -> ThrowsError LispVal
isSymbol = isType (AnyUnpacker unpackAtom)
isString = isType (AnyUnpacker unpackStr)
isNumber = isType (AnyUnpacker unpackNum)
isBool = isType (AnyUnpacker unpackBool)
isChar = isType (AnyUnpacker unpackAtom)

{-
Excercise 3.1.3 (DONE)
Add the symbol-handling functions from R5RS. A symbol is what
we've been calling an Atom in our data constructors
-}

symbolToString, stringToSymbol :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom content] = return $ String content
symbolToString _ = throwError $ Default "Only atoms permitted" 

stringToSymbol [String content] = return $ Atom content
stringToSymbol _ = throwError $ Default "Only strings permitted"

-- List primitives

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left _ -> False
      Right (Bool val) -> val
      Right _ -> False 
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func specs vargs corpus env) args =
  if num specs /= num args && isNothing vargs
    then throwError $ NumArgs (num specs) args
    else liftIO (bindVars env $ zip specs args) >>= bindVarArgs vargs >>= evalBody
  where
    remainingArgs = drop (length specs) args
    num = toInteger . length
    evalBody domain = last <$> mapM (eval domain) corpus 
    bindVarArgs arg domain = case arg of
      Just argName -> liftIO $ bindVars domain [(argName, List remainingArgs)]
      Nothing -> return domain 
apply (IOFunc func) args = func args
apply _ _ = throwError $ Default "No function passed"

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc _ = throwError $ Default "Expected procedure" 

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort _ badForm = throwError $ TypeMismatch "Expected string describing filename" $ List badForm 

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc badForm = throwError $ TypeMismatch "Expected port" $ List badForm

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc badForm = throwError $ TypeMismatch "Expected object and port" $ List badForm

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents badForm = throwError $ TypeMismatch "Expected string with filename" $ List badForm

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
readAll badForm = throwError $ TypeMismatch "Expected string with filename" $ List badForm

-- Primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", isSymbol),
    ("string?", isString),
    ("number?", isNumber),
    ("char?", isChar),
    ("boolean?", isBool),
    ("symbol->string", symbolToString),
    ("string->symbol", stringToSymbol),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]