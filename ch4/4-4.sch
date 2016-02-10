(define (and? exp) (tagged-list? exp 'and))

(define (and-clauses exp) (cdr exp))

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

(define (or? exp) (tagged-list? exp 'or))

(define (or-clauses exp) (cdr exp))

(define (eval-or exp env)
  (let ((clauses (or-clauses exp)))
    (if (no-clauses? clauses)
	false
	(let ((result (eval (first-clause clauses) env)))
	  (if result
	      result
	      (if (no-clauses (rest-clauses clauses))
		  false
		  (eval-or (rest-clauses clauses) env)))))))

;; as derive exp

(define (and->if exp)
  (foo (and-clauses exp)))

(define (foo clauses)
  (if (null? clauses)
      true
      (let ((first car clauses)
	    (rest (cdr clauses)))
	(if (null? rest)
	    (eval first)
	    (make-if first
		     first
		     (foo rest))))))
		     
