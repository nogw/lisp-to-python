(define (curry func arg1)  
  (lambda (arg) (apply func (cons arg1 (list arg)))))