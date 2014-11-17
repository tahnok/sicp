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

(define (make-sum a1 . a2)
  (cond
   ((null? a2) a1)
   ((=number? a1 0) (apply make-sum a2)
   ((and (number? a1) (number? a2)) (+ a1 a2)) 
   (else (list '+ a1 a2)))))

(define (make-product m1 m2 . m3)
  (if (pair? m3)
      (if (> (length m3) 1)
	  (apply make-product (make-product m1 m2) (car m3) (cdr m3))
	  (make-product (make-product m1 m2) (car m3)))
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
	((=number? m1 1) m2) 
	((=number? m2 1) m1) 
	((and (number? m1) (number? m2)) (* m1 m2)) 
	(else (list '* m1 m2)))))

(define (sum? x) 
  (and (pair? x) (eq? (car x) '+))) 

(define (addend s) (cadr s)) 

(define (augend s) (make-sum) 

(define (product? x) 
  (and (pair? x) (eq? (car x) '*))) 

(define (multiplier p) (cadr p)) 

(define (multiplicand p) (caddr p)) 
