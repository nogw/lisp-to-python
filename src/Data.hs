{-# LANGUAGE LambdaCase #-}

module Data where

import Control.Monad.Except (ExceptT, MonadError (catchError))
import Data.IORef
import System.IO (Handle)
import System.IO.Unsafe
import Text.Parsec (ParseError)

data Ast
  = Atom String
  | List [Ast]
  | DottedList [Ast] Ast
  | String String
  | Number Integer
  | Bool Bool
  | PrimitiveFunc ([Ast] -> Either Errors Ast)
  | Func {params :: [String], vararg :: Maybe String, body :: [Ast], closure :: IORef [(String, IORef Ast)]}
  | IOFunc ([Ast] -> IOThrowsError Ast)
  | Port Handle
  deriving (Show)

instance Show (a -> b) where -- temporary / debug
  show a = "func" -- temporary / debug

instance (Show a) => Show (IORef a) where -- temporary / debug
  show a = show (unsafePerformIO (readIORef a)) -- temporary / debug

-- instance Show Ast where show = showValue

data Errors
  = NumArgs Integer [Ast]
  | TypeMismatch String Ast
  | Parser ParseError
  | BadSpecialForm String Ast
  | NotFunction String String
  | UnboundVariable String String
  | Default String
  -- deriving (Show)

instance Show Errors where show = showError

type ThrowsError = Either Errors

type Env = IORef [(String, IORef Ast)]

type IOThrowsError = ExceptT Errors IO

showValue :: Ast -> String
showValue = \case
  Atom s -> s
  String s -> "\"" <> s <> "\""
  Number n -> show n
  Bool b -> if b then "#t" else "#f"
  List asts -> "(" <> showValueList asts <> ")"
  DottedList asts ast -> "(" <> showValueList asts <> showValue ast <> ")"
  PrimitiveFunc _ -> "<primitive>"
  Func{params = args, vararg = vararg, body = body, closure = env} ->
    "(lambda (" <> unwords (map show args) <> (case vararg of Nothing -> ""; Just arg -> " . " <> arg) ++ ") ...)"
  (Port _) -> "<IO port>"
  (IOFunc _) -> "<IO primitive>"
 where
  showValueList = unwords . map showValue

showError :: Errors -> String
showError = \case
  NumArgs exp found -> "Expected " <> show exp <> " args, found values " <> showValueList found
  TypeMismatch exp found -> "Invalid type: expect " <> exp <> ", found " <> show found
  Parser p_err -> "Parser error at " <> show p_err
  BadSpecialForm msg form -> msg <> ": " <> show form
  NotFunction msg func -> msg <> ": " <> func
  UnboundVariable msg var -> msg <> ": " <> var
  Default s -> show s
 where
  showValueList = unwords . map showValue

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = undefined