# transpiler

### TODO:

```scheme
(define (curry func arg1)  
  (lambda (arg) (apply func (cons arg1 (list arg)))))
```

`(cons arg1 (list arg))` in this case I need to join the argument list and pass it to function