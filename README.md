# transpiler

TODO:

def notf (x):
  if x:
    return False
  else:
    return True

def andf (lst):
  return fold(operator.and_, True, lst)

def foldl (func, accum, lst):
  if null(lst):
    return accum
  else:
    return foldl(func, func(accum, [lst][1]), [lst][1:])

fold = foldl

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))

(List [Atom "define", List [Atom "mem-helper", Atom "pred", Atom "op"],
  List [Atom "lambda", List [Atom "acc", Atom "next"],
    List [Atom "if", List [Atom "and", List [Atom "not",Atom "acc"], List [Atom "pred", List [Atom "op",Atom "next"]]],
      Atom "next",
      Atom "acc"]]])

def memhelper (pred, op):
  def scheme_lambda_x000 (acc, next):
    if andf(not(acc) pred(op(next))):
      return next
    else:
      return acc

```scheme
(define (max first . rest) 
  (fold (lambda (old new) (if (> old new) old new)) first rest))
```

```python
# INITIAL
def max (first, rest):
  return fold(def scheme_lambda_x000 (old, new):
    if (old) > (new):
      return old
    else:
      return new, first, rest)
```

```python
# FIRST
def max (first, rest):
  def scheme_lambda_x000 (old, new):
    if (old) > (new):
      return old
    else:
      return new, first, rest

  return fold()
```

```python
# SECOND
def max (first, rest):
  def scheme_lambda_x000 (old, new):
    if (old) > (new):
      return old
    else:
      return new, first, rest

  return fold(scheme_lambda_x000)
```

```haskell
  -- i need it 
  List [Atom "define",List [Atom "map",Atom "func",Atom "lst"],
    List [Atom "define",List [Atom "lambda_sex",Atom "x",Atom "y"],
      List [Atom "cons",List [Atom "func",Atom "x"],Atom "y"]],
      
    List [Atom "foldr",List [Atom "lambda_sex",List [Atom "quote",List []],Atom "lst"]]]

  -- pretty
  List [Atom "define",List [Atom "lambda_sex",Atom "x",Atom "y"],
  List [Atom "cons",List [Atom "func",Atom "x"],Atom "y"]],
  List [Atom "foldr",List [Atom "lambda_sex",List [Atom "quote",List []],Atom "lst"]]

  List [Atom "define",List [Atom "lambda_sex",Atom "x",Atom "y"],
  List [Atom "cons",List [Atom "func",Atom "x"],Atom "y"]],
  List [Atom "foldr",List [Atom "lambda_sex",List [Atom "quote",List []],Atom "lst"]]

  [List [Atom "define",List [Atom "lambda_sex",Atom "x",Atom "y"],
   List [Atom "cons",List [Atom "func",Atom "x"],Atom "y"]],
   List [Atom "define",List [Atom "lambda_sex",Atom "x",Atom "y"],
   List [Atom "cons",List [Atom "func",Atom "x"],Atom "y"]]]
```