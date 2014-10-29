(define (equal a b)
  (cond
   ((and (list? a) (list? b))
    (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
   ((and (not (pair? a)) (not (pair? b)))
    (eq? a b))
   (else
    false)))
