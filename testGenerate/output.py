def curry(func, arg1):
  return lambda arg: func(arg1, arg)