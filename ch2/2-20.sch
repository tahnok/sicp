(define (same-parity head . tail)
  (define (same-parity-iter comparator collector left)
    (cond
     ((null? left) (append (list head) collector))
     ((comparator (car left))
      (same-parity-iter comparator (append collector (list (car left))) (cdr left)))
     (else
      (same-parity-iter comparator collector (cdr left)))))
  (same-parity-iter (if (even? head) even? odd?) (list) tail))
