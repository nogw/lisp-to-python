# transpiler

### TODO:

```scheme
(define (curry func arg1)
  (lambda (arg) (apply func (cons arg1 (list arg)))))
```

`(cons arg1 (list arg))` in this case I need to join the argument list and pass it to function

---

```scheme
(define (foldr func end lst)
  (if (null? lst)
    end
    (func (car lst) (foldr func end (cdr lst)))))
```

```python
def foldl(func, accum, lst):
  if null(lst):
    return accum
  else:
    return foldl(func, func(accum, lst[0]), lst[1:])
```
