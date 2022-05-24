(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))

(define (null? obj) (if (eqv? obj '()) #t #f))

(define (foldl func accum lst) 
  (if (null? lst) 
    accum 
    (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)

(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))