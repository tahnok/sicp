;-------------------------- GIVEN
 
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2))) 

(define (=number? exp num) 
  (and (number? exp) (= exp num))) 


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
	((=number? e2 1) e1)
	(else (list '** e1 e2))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (deriv exp var) 
  (cond ((number? exp) 0) 
	((variable? exp)
	 (if (same-variable? exp var) 1 0)) 
	((sum? exp) 
	 (make-sum (deriv (addend exp) var) 
		   (deriv (augend exp) var))) 
	((product? exp) 
	 (make-sum 
	  (make-product (multiplier exp) 
			(deriv (multiplicand exp) var)) 
	  (make-product (deriv (multiplier exp) var) 
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product
	   (exponent exp)
	   (make-exponentiation
	    (base exp)
	    (make-sum (exponent exp) -1))
	   (deriv (base exp)))))
	(else 
	 (error "unknown expression type -- DERIV" exp))))

;--------------
;(make-sum 1 2 3 4) -> 10
;(make-sum 1 x y) -> (+ 1 x y)
;(make-sum x 1 2) -> (+ x 3) (not sure how to do this yet though)
;(make-sum 1) -> 1
;(make-sum x) -> 'x
;(make-sum (+ x y) z) -> (+ x y z)



(define (make-sum a1 . a2)
  (cond
   ((null? a2) a1)
   ((sum? a1) (append a1 (make-sum a2)))
   ((=number? a1 0) (apply make-sum a2))
   ((not (pair? (cdr a2))) ;a2 only has one element
    (if (and (number? a1) (number? (car a2)))
	(+ a1 (car a2))
	(list '+ a1 (car a2))))
   (else (apply make-sum (make-sum a1 (car a2)) (cdr a2)))))

(define (sum? x) 
   (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s)) 

(define (augend s) (make-sum) 

;------------
(define (make-product m1 m2 . m3)

(define (product? x) 

(define (multiplier p) (cadr p)) 

(define (multiplicand p) (caddr p)) 
