(define (reverse inlist)
  (if (null? (cdr inlist))
      inlist
      (append (reverse (cdr inlist)) (list (car inlist)))))
