{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Data
import Data.Functor ((<&>))
import Data.IORef
import Data.Maybe
import Parser
import System.IO

data Unpacker = forall a. Eq a => AnyUnpacker (Ast -> ThrowsError a)

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = \case
  Left e -> throwError e
  Right val -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> Data.Maybe.isJust . lookup var

getVar :: Env -> String -> IOThrowsError Ast
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVariable "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> Ast -> IOThrowsError Ast
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVariable "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> Ast -> IOThrowsError Ast
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, Ast)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
 where
  extendEnv bindings env = fmap (<> env) (mapM addBinding bindings)
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)

evaluate :: Env -> Ast -> IOThrowsError Ast
evaluate env = \case
  val@(String _) -> return val
  val@(Number _) -> return val
  val@(Bool _) -> return val
  (List [Atom "quote", val]) -> return val
  (List [Atom "if", pred, conseq, alt]) -> do
    result <- evaluate env pred
    case result of
      Bool False -> evaluate env alt
      _ -> evaluate env conseq
  (Atom id) -> getVar env id
  (List [Atom "set!", Atom var, form]) -> evaluate env form >>= setVar env var
  (List [Atom "define", Atom var, form]) -> evaluate env form >>= defineVar env var
  (List (Atom "define" : List (Atom var : params) : body)) -> makeNormalFunc env params body >>= defineVar env var
  (List (Atom "define" : DottedList (Atom var : params) varargs : body)) -> makeVarArgs varargs env params body >>= defineVar env var
  (List (Atom "lambda" : List params : body)) -> makeNormalFunc env params body
  (List (Atom "lambda" : DottedList params varargs : body)) -> makeVarArgs varargs env params body
  (List (Atom "lambda" : varargs@(Atom _) : body)) -> makeVarArgs varargs env [] body
  (List [Atom "load", String filename]) -> load filename >>= fmap last . mapM (evaluate env)
  (List (function : args)) -> do
    func <- evaluate env function
    argVals <- mapM (evaluate env) args
    apply func argVals
  badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Ast -> [Ast] -> IOThrowsError Ast
apply x args = case x of
  (PrimitiveFunc func) -> liftThrows $ func args
  (Func params varargs body closure) ->
    if num params /= num args && isNothing varargs
      then throwError $ NumArgs (num params) args
      else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
   where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (evaluate env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
  (IOFunc func) -> func args
  _ -> undefined

makeFunc varargs env params body = return $ Func (map showValue params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarArgs = makeFunc . Just . showValue

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= flip
      bindVars
      ( map (makeFunc IOFunc) ioPrimitives
          ++ map (makeFunc PrimitiveFunc) primitives
      )
 where
  makeFunc constructor (var, func) = (var, constructor func)

applyProc :: [Ast] -> IOThrowsError Ast
applyProc = \case
  [func, List args] -> apply func args
  (func : args) -> apply func args
  _ -> undefined

makePort :: IOMode -> [Ast] -> IOThrowsError Ast
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [Ast] -> IOThrowsError Ast
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc = \case
  [] -> readProc [Port stdin]
  [Port port] -> liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [Ast] -> IOThrowsError Ast
writeProc = \case
  [obj] -> writeProc [obj, Port stdout]
  [obj, Port port] -> liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [Ast] -> IOThrowsError Ast
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [Ast]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [Ast] -> IOThrowsError Ast
readAll [String filename] = List <$> load filename

primitives :: [(String, [Ast] -> ThrowsError Ast)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

ioPrimitives :: [(String, [Ast] -> IOThrowsError Ast)]
ioPrimitives =
  [ ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

numBoolBinop :: (Integer -> Integer -> Bool) -> [Ast] -> ThrowsError Ast
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [Ast] -> ThrowsError Ast
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [Ast] -> ThrowsError Ast
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> [Ast] -> ThrowsError Ast
numericBinop op params = case (op, params) of
  (op, []) -> throwError $ NumArgs 2 []
  (op, singleVal@[_]) -> throwError $ NumArgs 2 singleVal
  (op, params) -> mapM unpackNum params <&> (Number . foldl1 op)

boolBinop :: (Ast -> ThrowsError a) -> (a -> a -> Bool) -> [Ast] -> ThrowsError Ast
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker (head args)
      right <- unpacker (head . tail $ args)
      return $ Bool $ left `op` right

unpackNum :: Ast -> ThrowsError Integer
unpackNum = \case
  (Number n) -> return n
  (String n) ->
    let parsed = reads n :: [(Integer, String)]
     in if null parsed
          then throwError $ TypeMismatch "number" $ String n
          else return $ fst $ head parsed
  (List [n]) -> unpackNum n
  n -> throwError $ TypeMismatch "number" n

unpackStr :: Ast -> ThrowsError String
unpackStr = \case
  (String s) -> return s
  (Number s) -> return $ show s
  (Bool s) -> return $ show s
  n -> throwError $ TypeMismatch "string" n

unpackBool :: Ast -> ThrowsError Bool
unpackBool = \case
  (Bool b) -> return b
  n -> throwError $ TypeMismatch "boolean" n

unpackEquals :: Ast -> Ast -> Unpacker -> ThrowsError Bool
unpackEquals arg arg' (AnyUnpacker unpacker) =
  do
    unpacked <- unpacker arg
    unpacked' <- unpacker arg'
    return $ unpacked == unpacked'
    `catchError` const (return False)

equal :: [Ast] -> ThrowsError Ast
equal [arg, arg'] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg arg')
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg, arg']
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgsList = throwError $ NumArgs 2 badArgsList

car :: [Ast] -> ThrowsError Ast
car = \case
  [List (x : xs)] -> return x
  [DottedList (x : xs) _] -> return x
  [badArg] -> throwError $ TypeMismatch "pair" badArg
  badArgList -> throwError $ NumArgs 1 badArgList

cdr :: [Ast] -> ThrowsError Ast
cdr = \case
  [List (x : xs)] -> return $ List xs
  [DottedList [_] x] -> return x
  [DottedList (_ : xs) x] -> return $ DottedList xs x
  [badArg] -> throwError $ TypeMismatch "pair" badArg
  badArgList -> throwError $ NumArgs 1 badArgList

cons :: [Ast] -> ThrowsError Ast
cons = \case
  [x1, List []] -> return $ List [x1]
  [x, List xs] -> return $ List $ x : xs
  [x, DottedList xs xlast] -> return $ DottedList (x : xs) xlast
  [x1, x2] -> return $ DottedList [x1] x2
  badArgList -> throwError $ NumArgs 2 badArgList

eqv :: [Ast] -> ThrowsError Ast
eqv = \case
  [Bool arg, Bool arg'] -> return $ Bool $ arg == arg'
  [Number arg, Number arg'] -> return $ Bool $ arg == arg'
  [String arg, String arg'] -> return $ Bool $ arg == arg'
  [Atom arg, Atom arg'] -> return $ Bool $ arg == arg'
  [DottedList xs x, DottedList ys y] -> eqv [List $ xs <> [x], List $ ys <> [y]]
  [List arg, List arg'] -> return $ Bool $ (length arg == length arg') && all eqvPair (zip arg arg')
  [_, _] -> return $ Bool False
  badArgList -> throwError $ NumArgs 2 badArgList
 where
  eqvPair (x1, x2) =
    case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
