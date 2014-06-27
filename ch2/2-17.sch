(define (last-pair inlist)
  (if (null? inlist)
      (list)
      (list (list-ref inlist (- (length inlist) 1)))))
