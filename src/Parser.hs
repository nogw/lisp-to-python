module Parser where

import Control.Monad.Except (liftM, throwError)
import Data
import Data.Char
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Error

type Parser = Parsec String ()

symbol :: ParsecT String u Identity Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser Ast
parseString = String <$> between (char '"') (char '"') (many $ noneOf ['"'])

parseInt :: Parser Ast
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit 
  return $ Number (read (sign ++ digits) :: Integer)

parseQuote :: Parser Ast
parseQuote = do
  char '\''
  x <- parserT
  return $ List [Atom "quote", x]

parseAtom :: Parser Ast
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseList :: Parser Ast
parseList = List <$> sepBy parserT spaces

parseDottedList :: Parser Ast
parseDottedList = do
  head <- endBy parserT spaces
  tail <- char '.' >> spaces >> parserT
  return $ DottedList head tail

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError Ast
readExpr = readOrThrow parserT

readExprList :: String -> ThrowsError [Ast]
readExprList = readOrThrow (endBy parserT spaces)

parserT :: ParsecT String () Data.Functor.Identity.Identity Ast
parserT =
  try parseAtom
    <|> parseInt
    <|> parseString
    <|> parseQuote
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

run :: String -> Either ParseError Ast
run = parse parserT "lisp"