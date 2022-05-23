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
    aux acc ast = do
      case ast of
        Number a -> return ("(" <> show a <> ")")
        List [Atom op, a, b] -> do
          n1 <- aux acc a
          n2 <- aux acc b
          return $ "(" <> (n1 <> getOperator op <> n2) <> ")"
        List (Atom op : a : b : rest) -> do
          n1 <- aux acc a
          n2 <- aux acc b
          return $ accListOfNumbers op (n1 <> getOperator op <> n2) rest
        _ -> return acc
    accListOfNumbers :: String -> String -> [Ast] -> String
    accListOfNumbers op acc ns = case ns of
      ast1 : asts -> do
        n1 <- aux acc ast1
        accListOfNumbers op ("(" <> acc <> ")" <> getOperator op <> n1) asts
      [] -> "(" <> acc <> ")"

testArith :: IO ()
testArith = do
  ctx <- emptyCtx
  let expr = List [Atom "+", Number 1, Number 2, Number 3, Number 4, Number 5]
  gen <- generateArithExpression ctx expr
  putStrLn "\n"
  print gen

{-
  AST: List [Atom "if", List [Atom ">", Number 1, Number 2], List [Atom "+", Number 1, Number 2], List [Atom "-", Number 2, Number 1]]
  EXPECT(FAIL): if ((2) > (1)):
            ((2) + (1))
          else:
            ((1) - (2))

  AST: List [Atom "if", List [Atom ">", Number 10, Number 20], List [Atom "if", List [Atom "<", Number 10, Number 15], List [Atom "+", Number 10, Number 12], List [Atom "-", Number 10, Number 12]], List [Atom "-", Number 2, Number 1]]
  EXPECT(FAIL): if (10) > (20):
                  return if (10) < (15):
                    return (10) + (12)
                  else:
                    return (10) - (12)
                else:
                  return (2) - (1)
-}

generateIfStatement :: Ctx -> Ast -> Ast -> Ast -> IO String
generateIfStatement ctx pred conseq alt = do
  cond <- generate ctx pred
  conseq' <- generate ctx conseq
  alt' <- generate ctx alt
  return $
    "if " <> cond <> ":\n"
      <> indent ("return " <> conseq')
      <> "\nelse:\n"
      <> indent ("return " <> alt')

-- printf
--   "if %s:\n%s\nelse:\n%s"
--   (generate ctx pred)
--   (indent $ "return " <> conseq')
--   (indent $ "return " <> alt')

{-
  List [Atom "define", Atom "x", Number 1]
  x = 1
  List [
    Atom "define", List [Atom "f", Atom "x", Atom "y"],
      List [Atom "define", Atom "z", List [Atom "+", Atom "x", Atom "y"]],
      List [Atom "*", Atom "z", Atom "z"]
    ]
  def f (x,y):
    z = ((x) + (y))
    return ((z) * (z))
  List [
    Atom "define", List [Atom "f", Atom "x", Atom "y"],
      List [Atom "define", Atom "z", List [Atom "+", Atom "x", Atom "y"]],
      List [Atom "define", Atom "z1", List [Atom "-", Atom "x", Atom "y"]],
      List [Atom "*", Atom "z", Atom "z1"]
  ]
  def f (x,y):
    z = ((x) + (y))
    z1 = ((x) - (y))
    return ((z) * (z1))
-}

generateVariable :: Ctx -> String -> Ast -> IO String
generateVariable ctx var body =
  -- printf "%s = %s" var (generate ctx body)
  do
    bd <- generate ctx body
    return $ var <> " = " <> bd

{-
  CODE: (define (flip func) (lambda (arg arg1) (func arg1 arg)))
  AST: (List [Atom "define", List [Atom "flip", Atom "func"], List [Atom "lambda", List [Atom "arg",Atom "arg1"], List [Atom "func", Atom "arg1", Atom "arg"]]])
  EXPECT: def flip (func):
            return lambda arg, arg1: func(arg1, arg)

  CODE: ((lambda (a b) (+ (* 2 a) b)) 5 6)
  AST: (List [List [Atom "lambda", List [Atom "a",Atom "b"], List [Atom "+", List [Atom "*",Number 2,Atom "a"], Atom "b"]]])
  EXPECT: (lambda a, b: ((2) * (a)) + (b))(5, 6)

  CODE: (define (compose f g) (lambda (arg) (f (apply g arg))))
  AST: (List [Atom "define", List [Atom "compose", Atom "f", Atom "g"], List [Atom "lambda", List [Atom "arg"], List [Atom "f", List [Atom "apply", Atom "g", Atom "arg"]]]])
  EXPECT: def compose (f, g):
            return lambda arg: f(g(arg))

  CODE: (define (foldr func end lst) (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))
  AST: (List [Atom "define", List [Atom "foldr", Atom "func", Atom "end", Atom "lst"], List [Atom "if", List [Atom "null?", Atom "lst"], Atom "end", List [Atom "func", List [Atom "car", Atom "lst"], List [Atom "foldr", Atom "func", Atom "end", List [Atom "cdr", Atom "lst"]]]]])
  EXPECT: def foldr (func, end, lst):
            if null(lst):
              return end
            else:
              return func([lst][1], foldr(func, end, [lst][1:]))

  CODE: (define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
  AST: List [Atom "define",List [Atom "mem-helper",Atom "pred",Atom "op"],List [Atom "lambda",List [Atom "acc",Atom "next"],List [Atom "if",List [Atom "and",List [Atom "not",Atom "acc"],List [Atom "pred",List [Atom "op",Atom "next"]]],Atom "next",Atom "acc"]]]
  ECPECT(FAIL): def memhelper(pred, op):
                  def scm_lambda_125(acc, next):
                    if py_keyword_and(py_keyword_not(acc), pred(op(next))):
                      return next
                    else:
                      return acc
-}

generateLambda :: Ctx -> [Ast] -> [Ast] -> IO String
generateLambda ctx params body =
  if canBeLambdaBody (head body)
    then do
      body' <- mapM (generate ctx) body
      params' <- mapM (generate ctx) params
      return $
        printf
          "lambda %s: %s"
          (joinComma params')
          (joinNewLine body')
    else generateFunction ctx "scm_lambda_125" params body

{-
  CODE: (define (unfold func init pred) (if (pred init) (cons init '()) (cons init (unfold func (func init) pred))))
  AST: List [Atom "define",List [Atom "unfold",Atom "func",Atom "init",Atom "pred"],List [Atom "if",List [Atom "pred",Atom "init"],List [Atom "cons",Atom "init",List [Atom "quote",List []]],List [Atom "cons",Atom "init",List [Atom "unfold",Atom "func",List [Atom "func",Atom "init"],Atom "pred"]]]]
  EXPECT: def unfold (func, init, pred):
            if pred(init):
              return [init]
            else:
              return [init, unfold(func, func(init), pred)]
-}

generateCons :: Ctx -> [Ast] -> [String] -> IO String
generateCons ctx x acc =
  case x of
    [List (Atom "cons" : x' : x1')] -> do
      rest <- generate ctx x'
      generateCons ctx x1' (acc <> [rest])
    [List [Atom "quote", List []]] -> generateCons ctx (tail x) acc
    [List _] -> do
      rest <- mapM (generate ctx) x
      generateCons ctx [] $ acc <> rest
    [Atom x] -> generateCons ctx [] (acc <> [x])
    [] -> return $ "[" <> joinComma acc <> "]"
    _ -> undefined

-- _ -> do
--   lst <- joinComma <$> mapM (generate ctx) x
--   return $ "[" <> lst <> "]"

generateQuoteList :: Ctx -> Ast -> IO String
generateQuoteList ctx val = case val of
  List lst ->
    do
      lst' <- joinComma <$> mapM (generate ctx) lst
      return $ "[" <> lst' <> "]"
  _ -> undefined

{-
  CODE: (define (sum . lst) (fold + 0 lst))
  AST: List [Atom "define",DottedList [Atom "sum"] (Atom "lst"),List [Atom "fold",Atom "+",Number 0,Atom "lst"]]
  EXPECT: def sum(lst):
            fold(operator.add, 0, lst)
-}

{-
  CODE: (define (f a b) (+ a b))
  AST: List [Atom "define", List [Atom "f", Atom "a", Atom "b"], List [Atom "+", Atom "a", Atom "b"]]
  EXPECT: def f(a, b):
            (a) + (b)

  CODE: (define (s a b c d e f g h) (+ a b c d e f g h))
  AST: List [Atom "define",List [Atom "s",Atom "a",Atom "b",Atom "c",Atom "d",Atom "e",Atom "f",Atom "g",Atom "h"],List [Atom "+",Atom "a",Atom "b",Atom "c",Atom "d",Atom "e",Atom "f",Atom "g",Atom "h"]]
  EXPECT: def s(a, b, c, d, e, f, g, h):
            (((((((g) + (h) + (f)) + (e)) + (d)) + (c)) + (b)) + (a))
-}

generateFunction :: Ctx -> String -> [Ast] -> [Ast] -> IO String
generateFunction ctx name params body =
  do
    pushFunct name ctx
    params' <- joinComma <$> mapM (generate ctx) params
    body' <- do indent . joinNewLine <$> generateBodyFunc ctx body []
    return $ "def " <> filterName name <> "(" <> params' <> "):\n" <> body'

generateBodyFunc :: Ctx -> [Ast] -> [String] -> IO [String]
generateBodyFunc ctx body' xs = do
  case body' of
    [] -> return ["()"]
    [f] -> do
      traceM $ show body'
      traceM $ show (canBeReturn f)
      traceM "\n"
      x' <- generate ctx f

      -- check if list have some function as argument, because if nothing argument can be a function, this give Lists that haven a function
      return $ xs <> [x']
    f : rest -> do
      body1 <- generate ctx f
      generateBodyFunc ctx rest (xs <> [body1])

canBeReturn :: Ast -> Bool
canBeReturn arg =
  case arg of
    List [Atom "define", _, _] -> False
    List [Atom "if", _, _, _] -> False
    _ -> True

{-
(define (mem-helper pred op)
  (lambda (acc next) (if (and (not acc) (pred (op next)))
    next
    acc)))

List [Atom "define", List [Atom "mem-helper", Atom "pred", Atom "op"],
  List [Atom "lambda", List [Atom "acc", Atom "next"],
    List [Atom "if",
      List [Atom "and",
        List [Atom "not", Atom "acc"],
        List [Atom "pred", List [Atom "op",Atom "next"]]],Atom "next",Atom "acc"]]])
-}

{-
  iter args

  check if is lambda, if so,
  generate and sub name of lambda to call a function
-}

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
            do
              let fn = "scm_lambda_" <> show (length funcs)
              checkArgs rest (funcs <> [List (Atom "define" : List (Atom fn : params) : body)], args <> [Atom fn])
          _ -> undefined
    _ -> (funcs, args)
  where
    canBeUsedAsArg :: Ast -> Bool
    canBeUsedAsArg = \case
      List [Atom "lambda", _, _] -> False
      _ -> True

{-
  [List [Atom "foldr",List [Atom "lambda",List [Atom "x",Atom "y"],List [Atom "cons",List [Atom "func",Atom "x"],Atom "y"]],List [Atom "quote",List []],Atom "lst"]]

  List [Atom "lambda",List [Atom "x",Atom "y"],List [Atom "cons",List [Atom "func",Atom "x"],Atom "y"]]

  [List [Atom "foldr", Atom "lambda_gen_121",List [Atom "quote",List []],Atom "lst"]]
-}

checkedArgs :: ([Ast], [Ast])
checkedArgs = do
  checkArgs
    [Atom "foldr", List [Atom "lambda", List [Atom "x", Atom "y"], List [Atom "cons", List [Atom "func", Atom "x"], Atom "y"]], List [Atom "lambda", List [Atom "x", Atom "y"], List [Atom "cons", List [Atom "func", Atom "x"], Atom "y"]], List [Atom "quote", List []], Atom "lst"]
    ([], [])

parseArg :: Ctx -> Ast -> IO String
parseArg ctx arg =
  case arg of
    Atom a -> generate ctx (Atom a)
    List [Atom a, List b] -> generateCallFunction ctx a [Atom "b"]
    List (Atom a : b) ->
      if canBeArgument $ List (Atom a : b)
        then
          if isOperator a
            then do
              generateCallFunction ctx (fromMaybe a (lookup a operatorsWhichCanBeArguments)) b
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
      lst <- generate ctx f
      params <- joinComma <$> mapM (generate ctx) rest
      return $ printf "(" <> lst <> ")(" <> params <> ")"

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
    lst <- joinComma <$> mapM (generate ctx) arg
    return $ lst <> "[0]"
  List (Atom "cdr" : arg) -> do
    lst <- joinComma <$> mapM (generate ctx) arg
    return $ lst <> "[1:]"
  List (Atom "list" : rest) -> do
    lst <- joinComma <$> mapM (generate ctx) rest
    return $ "[" <> lst <> "]"
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
  let filt_n = [x | x <- name', x `notElem` ",.?!-:;\"\'"]
   in checkName filt_n
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

pushFunct :: String -> Ctx -> IO ()
pushFunct func Ctx {functions = s, ..} = do modifyIORef s (func :)

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