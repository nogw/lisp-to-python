import operator

def py_keyword_not(x):
  if x:
    return False
  else:
    return True

def null(obj):
  if ((obj)==([])):
    return True
  else:
    return False

def list(objs):
  return objs

def id(obj):
  return obj

def flip(func):
  return lambda arg, arg1: func(arg1, arg)

def curry(func, arg1):
  return lambda arg: func(cons(arg1, [arg]))

def compose(f, g):
  return lambda arg: f(g(arg))

zero = curry(operator.eq, 0)

positive = curry(operator.lt, 0)

negative = curry(operator.gt, 0)

def odd(num):
  return ((((num)%(2)))==(1))

def even(num):
  return ((((num)%(2)))==(0))

def foldr(func, end, lst):
  if null(lst):
    return end
  else:
    return func(lst[0], foldr(func, end, lst[1:]))

def foldl(func, accum, lst):
  if null(lst):
    return accum
  else:
    return foldl(func, func(accum, lst[0]), lst[1:])

fold = foldl

reduce = foldr

def unfold(func, init, pred):
  if pred(init):
    return [init]
  else:
    return [init, unfold(func, func(init), pred)]

def sum(lst):
  return fold(operator.add, 0, lst)

def product(lst):
  return fold(operator.mul, 1, lst)

def py_keyword_and(lst):
  return fold(operator.and_, True, lst)

def py_keyword_or(lst):
  return fold(operator.or_, False, lst)

def max(first, rest):
  def scm_lambda_0(old, new):
    if ((old)>(new)):
      return old
    else:
      return new
  
  return fold(scm_lambda_0, first, rest)

def min(first, rest):
  def scm_lambda_0(old, new):
    if ((old)<(new)):
      return old
    else:
      return new
  
  return fold(scm_lambda_0, first, rest)

def length(lst):
  def scm_lambda_0(x, y):
    return ((x)+(1))
  
  return fold(scm_lambda_0, 0, lst)

def reverse(lst):
  return fold(flip(cons), [], lst)

def memhelper(pred, op):
  def scm_lambda_2(acc, next):
    if py_keyword_and(py_keyword_not(acc), pred(op(next))):
      return next
    else:
      return acc

def memq(obj, lst):
  return fold(memhelper(curry(eq, obj), id), False, lst)

def memv(obj, lst):
  return fold(memhelper(curry(eqv, obj), id), False, lst)

def member(obj, lst):
  return fold(memhelper(curry(equal, obj), id), False, lst)

def assq(obj, alist):
  return fold(memhelper(curry(eq, obj), car), False, alist)

def assv(obj, alist):
  return fold(memhelper(curry(eqv, obj), car), False, alist)

def assoc(obj, alist):
  return fold(memhelper(curry(equal, obj), car), False, alist)

def map(func, lst):
  def scm_lambda_0(x, y):
    return [func(x), y]
  
  return foldr(scm_lambda_0, [], lst)

def filter(pred, lst):
  def scm_lambda_0(x, y):
    if pred(x):
      return [x, y]
    else:
      return y
  
  return foldr(scm_lambda_0, [], lst)

def fib(n):
  if ((n)<=(2)):
    return 1
  else:
    return ((fib(((n)-(1))))+(fib(((n)-(2)))))