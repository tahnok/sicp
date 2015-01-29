(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp))
	       (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr))

					;a)
					; We are looking up the rule for derivation based on the operator in the expression, and then dispatching based on the type to a deriv-operator method basically
					;we can't do the same thing for number? and variable? since there is no 'tag' on the data


(define (install-deriv-sums)

					;stuff
  )
