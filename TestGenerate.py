def memhelper (pred, op):
  def scheme_lambda_x000 (acc, next):
    if and(not(acc), pred(op, next)):
      return next
    else:
      return acc