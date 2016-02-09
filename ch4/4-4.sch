(define (and? exp) (tagged-list? exp 'and))

(define (and-clauses exp) (cd exp))

(define (no-clauses? clauses) (null? exp))

(define (first-clause clauses) (car clauses))

(define (rest-clauses clauses) (cdr clauses))

(define (eval-and exp env)
  (let ((clauses (and-clauses exp)))
    (if (no-clauses? clauses)
	true
	(let ((result (eval (first-clause clauses) env)))
	  (if result
	      (if (no-clauses (rest-clauses clauses))
		  result
		  (eval-and (rest-clauses clauses) env))
	      false)))))
