{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Generate where

import Data
import Data.IORef
import Data.List as List
import Data.Maybe
import Debug.Trace
import Parser
import Text.Printf

generateArithExpression :: Ctx -> Ast -> IO String
generateArithExpression ctx ast = aux "" ast
  where
    aux acc ast =
      case ast of
        Number a -> return ("(" <> show a <> ")")
        Atom a -> return ("(" <> a <> ")")
        List [Atom op, a, b] -> do
          n1 <- generate ctx a
          n2 <- generate ctx b
          return $ "(" <> ("(" <> n1 <> ")" <> getOperator op <> "(" <> n2 <> ")") <> ")"
        List (Atom op : a : b : rest) -> do
          n1 <- generate ctx a
          n2 <- generate ctx b
          accListOfNumbers op ("(" <> n1 <> ")" <> getOperator op <> "(" <> n2 <> ")") rest
        _ -> return acc
    accListOfNumbers :: String -> String -> [Ast] -> IO String
    accListOfNumbers op acc ns = case ns of
      ast1 : asts -> do
        aux acc ast1 >>= \n -> accListOfNumbers op ("(" <> acc <> ")" <> getOperator op <> n) asts
      [] -> return $ "(" <> acc <> ")"

generateIfStatement :: Ctx -> Ast -> Ast -> Ast -> IO String
generateIfStatement ctx cond conseq alt =
  generate ctx cond >>= \c ->
    generate ctx conseq >>= \cs ->
      generate ctx alt >>= \a ->
        return $ "if " <> c <> ":\n" <> (indent "return " <> cs) <> "\nelse:\n" <> (indent "return " <> a)

generateVariable :: Ctx -> String -> Ast -> IO String
generateVariable ctx var body = generate ctx body >>= \b -> return $ var <> " = " <> b

generateLambda :: Ctx -> [Ast] -> [Ast] -> IO String
generateLambda ctx params body =
  if canBeLambdaBody (head body)
    then do
      b <- mapM (generate ctx) body
      p <- mapM (generate ctx) params
      -- is it ok to always concatenate "return"?
      return $ "return lambda " <> joinComma p <> ": " <> joinNewLine b
    else
      let fn = "scm_lambda_" <> show (length params)
       in generateFunction ctx fn params body

generateCons :: Ctx -> [Ast] -> [String] -> IO String
generateCons ctx x acc =
  case x of
    [List (Atom "cons" : x' : xs)] ->
      generate ctx x' >>= \r -> generateCons ctx xs (acc <> [r])
    [List [Atom "quote", List []]] ->
      generateCons ctx (tail x) acc
    [List _] ->
      mapM (generate ctx) x >>= \r -> generateCons ctx [] $ acc <> r
    [Atom x] ->
      generateCons ctx [] (acc <> [x])
    [] ->
      return $ "[" <> joinComma acc <> "]"
    _ -> undefined

generateQuoteList :: Ctx -> Ast -> IO String
generateQuoteList ctx val = case val of
  List list -> mapM (generate ctx) list >>= (\l -> return $ "[" <> l <> "]") . joinComma
  _ -> undefined

generateFunction :: Ctx -> String -> [Ast] -> [Ast] -> IO String
generateFunction ctx name params body =
  addFnName name ctx
    >> joinComma <$> mapM (generate ctx) params >>= \p ->
      generateBodyFunc ctx body []
        >>= (\b -> return $ "def " <> filterName name <> "(" <> p <> "):\n" <> b)
          . indent
          . joinNewLine

checkIfHasFunction :: [Ast] -> Bool
checkIfHasFunction ast = do
  case ast of
    [] -> False
    h : t ->
      case h of
        Atom at -> (at == "lambda") || checkIfHasFunction t
        List asts -> checkIfHasFunction asts
        _ -> checkIfHasFunction t

canBeReturn :: Ast -> Bool
canBeReturn arg =
  case arg of
    List [Atom "define", _, _] -> False
    List [Atom "if", _, _, _] -> False
    _ -> True

generateBodyFunc :: Ctx -> [Ast] -> [String] -> IO [String]
generateBodyFunc ctx body xs = do
  case body of
    [] -> return ["()"]
    [f] ->
      generate ctx f >>= \f' ->
        if canBeReturn f && not (checkIfHasFunction body)
          then return $ xs <> ["return " <> f']
          else return $ xs <> [f']
    f : rest ->
      generate ctx f >>= \b ->
        generateBodyFunc ctx rest (xs <> [b])

generateCallFunction :: Ctx -> String -> [Ast] -> IO String
generateCallFunction ctx f args = do
  case checkArgs args ([], []) of
    ([], args') -> do
      args'' <- mapM (generate ctx) args'
      return $ filterName f <> "(" <> joinComma args'' <> ")"
    (funcs, args') -> do
      let body = funcs <> [List (Atom f : args')]
      List.intercalate "\n\n" <$> generateBodyFunc ctx body []

checkArgs :: [Ast] -> ([Ast], [Ast]) -> ([Ast], [Ast])
checkArgs s (funcs, args) =
  case s of
    a : rest ->
      if canBeUsedAsArg a
        then checkArgs rest (funcs, args <> [a])
        else case a of
          List (Atom "lambda" : List params : body) ->
            let fn = "scm_lambda_" <> show (length funcs)
             in checkArgs rest (funcs <> [List (Atom "define" : List (Atom fn : params) : body)], args <> [Atom fn])
          _ -> undefined
    _ -> (funcs, args)
  where
    canBeUsedAsArg :: Ast -> Bool
    canBeUsedAsArg = \case
      List [Atom "lambda", _, _] -> False
      _ -> True

parseArg :: Ctx -> Ast -> IO String
parseArg ctx arg =
  case arg of
    Atom a -> generate ctx (Atom a)
    List [Atom a, List b] -> generateCallFunction ctx a [Atom "b"]
    List (Atom a : b) ->
      if canBeArgument $ List (Atom a : b)
        then
          if isOperator a
            then generateCallFunction ctx (fromMaybe a (lookup a operatorsWhichCanBeArguments)) b
            else generateCallFunction ctx a b
        else generate ctx $ List (Atom a : b)
    _ -> generate ctx arg

generateList :: Ctx -> Ast -> [Ast] -> Ast -> IO String
generateList ctx f rest ast =
  case f of
    Atom f ->
      case lookup f operators of
        Nothing -> generateCallFunction ctx f rest
        Just _ -> generateArithExpression ctx ast
    _ -> do
      list <- generate ctx f
      params <- joinComma <$> mapM (generate ctx) rest
      return $ printf "(" <> list <> ")(" <> params <> ")"

generate :: Ctx -> Ast -> IO String
generate ctx ast = case ast of
  String val -> return val
  Number val -> return $ show val
  Bool val -> return $ show val
  Atom x ->
    let n = filterName x
     in if isOperator n
          then do
            addImport "operator" ctx
            return $ fromMaybe n (lookup n operatorsWhichCanBeArguments)
          else return n
  List [Atom "quote", val] -> generateQuoteList ctx val
  List [Atom "if", pred, conseq, alt] -> generateIfStatement ctx pred conseq alt
  List (Atom "define" : List (Atom var : params) : body) -> generateFunction ctx (filterName var) params body
  List (Atom "define" : DottedList (Atom var : params) varargs : body) -> generateFunction ctx (filterName var) (params <> [varargs]) body
  List [Atom "define", Atom var, form] -> generateVariable ctx (filterName var) form
  List (Atom "lambda" : List params : body) -> generateLambda ctx params body
  List (Atom "apply" : f : arg) -> do
    name <- filterName <$> generate ctx f
    args <- joinComma <$> mapM (parseArg ctx) arg
    return $ name <> "(" <> args <> ")"
  List (Atom "car" : arg) -> do
    list <- joinComma <$> mapM (generate ctx) arg
    return $ list <> "[0]"
  List (Atom "cdr" : arg) -> do
    list <- joinComma <$> mapM (generate ctx) arg
    return $ list <> "[1:]"
  List (Atom "list" : rest) -> do
    list <- joinComma <$> mapM (generate ctx) rest
    return $ "[" <> list <> "]"
  List (Atom "cons" : x : x') -> generateCons ctx [ast] []
  List (f : rest) -> generateList ctx f rest ast
  _ -> undefined

canBeLambdaBody :: Ast -> Bool
canBeLambdaBody f = case f of
  List [Atom "if", _, _, _] -> False
  _ -> True

isOperator :: String -> Bool
isOperator op = case lookup op operators of
  Nothing -> False
  Just _ -> True

getOperator :: String -> String
getOperator op = fromMaybe "" (lookup op operators)

extract :: Ctx -> Ast -> IO String
extract ctx ex = case ex of
  Atom x -> return x
  Number x -> return $ show x
  List [Atom _, _, _] -> generateArithExpression ctx ex
  List [Atom "quote", _] -> generate ctx ex
  _ -> undefined

filterName :: String -> String
filterName name' =
  checkName [x | x <- name', x `notElem` ",.?!-:;\"\'"]
  where
    checkName name =
      if name `elem` pythonReservedKeyword
        then "py_keyword_" <> name
        else name

canBeArgument :: Ast -> Bool
canBeArgument arg =
  case arg of
    List [Atom "lambda", _, _] -> False
    List [Atom "if", _, _, _] -> False
    _ -> True

indent :: String -> String
indent = List.intercalate "\n" . map ("  " <>) . lines

joinComma :: [String] -> String
joinComma = List.intercalate ", "

joinNewLine :: [String] -> String
joinNewLine = List.intercalate "\n"

writeOutputPython :: String -> IO ()
writeOutputPython = writeFile "testGenerate/output.py"

operatorsWhichCanBeArguments :: [(String, String)]
operatorsWhichCanBeArguments =
  [ ("+", "operator.add"), -- ok
    ("-", "operator.sub"), -- ok
    ("*", "operator.mul"), -- ok
    ("/", "operator.div"), -- ok
    ("=", "operator.eq"), -- ok
    ("<", "operator.lt"), -- ok
    (">", "operator.gt"), -- ok
    ("/=", "operator.ne"), -- ok
    (">=", "operator.ge"), -- ok
    ("<=", "operator.le"), -- ok
    ("&&", "operator.and_"), -- ok
    ("||", "operator.or_"), -- ok
    ("eq?", "operator.eq"), -- ok
    ("eqv?", "operator.eq"), -- ok
    ("equal?", "operator.eq") -- ok
  ]

operators :: [(String, String)]
operators =
  [ ("+", "+"), -- ok
    ("-", "-"), -- ok
    ("*", "*"), -- ok
    ("/", "/"), -- ok
    ("mod", "%"), -- ok / partial
    ("quotient", "//"), -- ok
    ("remainder", "%"), -- ok / partial
    ("=", "=="), -- ok
    ("<", "<"), -- ok
    (">", ">"), -- ok
    ("/=", "/="), -- ok
    (">=", ">="), -- ok
    ("<=", "<="), -- ok
    ("&&", "&&"), -- ok
    ("||", "||"), -- ok
    ("string=?", "=="), -- ok
    ("string<?", "<"), -- ok
    ("string>?", ">"), -- ok
    ("string<=?", "<="), -- ok
    ("string>=?", ">="), -- ok
    ("car", "car"),
    ("cdr", "cdr"),
    ("cons", "cons"),
    ("eq?", "=="), -- ok / partial
    ("eqv?", "=="), -- ok / partial
    ("equal?", "==") -- ok / partial
  ]

pythonReservedKeyword :: [String]
pythonReservedKeyword =
  [ "False",
    "def",
    "if",
    "raise",
    "None",
    "del",
    "import",
    "return",
    "True",
    "elif",
    "in",
    "try",
    "and",
    "else",
    "is",
    "while",
    "as",
    "except",
    "lambda",
    "with",
    "assert",
    "finally",
    "nonlocal",
    "yield",
    "break",
    "for",
    "not",
    "class",
    "from",
    "or",
    "continue",
    "global",
    "pass"
  ]

data Ctx = Ctx {functions :: IORef [String], imports :: IORef [String]} deriving (Show)

emptyCtx :: IO Ctx
emptyCtx = do
  fl <- newIORef []
  im <- newIORef []
  return Ctx {functions = fl, imports = im}

addFnName :: String -> Ctx -> IO ()
addFnName func Ctx {functions = s, ..} = do modifyIORef s (func :)

addImport :: String -> Ctx -> IO ()
addImport func Ctx {imports = s, ..} =
  readIORef s >>= \s' ->
    if func `elem` s'
      then return ()
      else modifyIORef s (func :)

showState :: Ctx -> IO ()
showState Ctx {functions = f, imports = i} = do
  f' <- readIORef f
  i' <- readIORef i
  printf "{ functions = [%s], imports = [%s] }\n" (joinComma f') (joinComma i')

getImports :: Ctx -> IO [String]
getImports Ctx {imports = i, ..} = do readIORef i

getLastFunc :: Ctx -> IO String
getLastFunc Ctx {functions = f, ..} = do
  f' <- readIORef f
  return $ head f'

runGenerator :: Ast -> IO ()
runGenerator ast = do
  ctx <- emptyCtx
  gen <- generate ctx ast
  imp <- getImports ctx

  if not (null imp)
    then writeOutputPython $ printf "%s\n\n%s" (joinNewLine (map ("import " <>) imp)) gen
    else writeOutputPython gen

genPythonCode :: String -> IO ()
genPythonCode code = case readExpr code of
  Left er -> error $ show er
  Right ast -> runGenerator ast

genPythonCodeFromFile :: String -> IO ()
genPythonCodeFromFile filename =
  emptyCtx >>= \ctx ->
    readFile filename
      >>= \c -> case readExprList c of
        Left er -> error $ show er
        Right asts -> do
          gAst <- mapM (generate ctx) asts
          showState ctx
          imp <- getImports ctx
          if not (null imp)
            then writeOutputPython $ joinNewLine (map ("import " <>) imp) <> "\n\n" <> List.intercalate "\n\n" gAst
            else writeOutputPython $ List.intercalate "\n\n" gAst