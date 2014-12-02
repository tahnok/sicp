(define (adjoin-set x set)
  (cond
   ((= x (car set)) set)
   ((< x (car set)) (cons x set))
   (else
    (cons (car x) (adjoin-set x (cdr set))))))
