def null(obj):
  if ((obj)==([])):
    return True
  else:
    return False

def foldl(func, accum, lst):
  if null(lst):
    return accum
  else:
    return foldl(func, func(accum, lst[0]), lst[1:])

fold = foldl

def length(lst):
  def scm_lambda_0(x, y):
    return ((x)+(1))
  
  return fold(scm_lambda_0, 0, lst)