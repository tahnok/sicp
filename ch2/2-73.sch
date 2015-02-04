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

;b
(define (install-deriv-sum)
  (define (addend exp)
    (cadr exp))
  (define (augend exp)
    (caddr exp))
  (define (make-sum exp1 exp2)
    (list '+ exp1 exp2))
  (define (deriv-sum args var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
    
  (put 'deriv '(+) deriv-sum))

(define (install-deriv-product)
  (define (multiplier exp)
    (cadr exp))
  (define (multiplicand exp)
    (caddr exp))
  (define (make-product exp1 exp2)
    (list '* exp1 exp2))
  (define (deriv-sum args var)
    (make-sum
     (make-product
      (multiplier exp)
      (deriv (multiplicand exp) var))
     (make-product
      (mutiplicand exp)
      (deriv (multiplier exp) var))
    ))
    
  (put 'deriv '(*) deriv-product))

;c
(define (install-deriv-exponent)
  (define (base exp) (card exp))
  (define (exponent exp) (caddr exp))
  (define (make-exponentiation e1 e2)
    (cond ((=number? e2 0) 1)
	  ((=number? e2 1) e1)
	  (else (list '** e1 e2))))

  (define (deriv-exponent args var)
    (make-product
     (make-product
      (exponent exp)
      (make-exponentiation
       (base exp)
       (make-sum (exponent exp) -1))
      (deriv (base exp)))))
  (put 'deriv '(**) deriv-exponent))

;d
