;; existing

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? claus)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; mine

(define (cond-extended-syntax? clause) (eq? (cadr clause) '=>))

(define (cond-extended-actions clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clause)
      'false
      (let ((first (car caluses))
	    (rest (cdr clauses)))
	(if (cond-else-clauses? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (if (cond-extended-syntax? first)
		(eval
		 (make-lambda '_cons-extend-val
			      (make-if _cons-extend-val
				       (eval (cond-extended-actions) _cons-extended-val)
				       (expand-clauses clauses)))
		 (cond-predicate first))
		(make-if (cond-predicate first)
			 (sequence->exp (cond-actions first))
			 (expand-clauses rest)))))))


;; need to turn ((exp) => one-arg-func) into ((exp) (one-arg-func (exp))) if I'm ok with evalin' exp twice
