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
   ((sum? a1)
    (if (= (length a2) 1)
	(append a1 a2)
	(append a1 (cdr (apply make-sum a2)))))
   ((=number? a1 0) (apply make-sum a2))
   ((not (pair? (cdr a2)))
    (if (nand (number? a1) (number? (car a2)))
	(+ a1 (car a2))
	(list '+ a1 (car a2))))
   (else (apply make-sum (make-sum a1 (car a2)) (cdr a2)))))


(define (sum? x) 
   (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (apply make-sum (cddr s)))

;------------
(define (make-product m1 . m2)
  (cond
   ((null? m2) m1)
   ((product? m1)
    (if (= (length m2) 1)
	(append m1 m2)
	(append m1 (cdr (apply make-product m2)))))
   ((or
     (=number? m1 0)
     (any (lambda (x) (=number? x 0)) m2))
    0) ;contains a 0
   ((=number? m1 1) (apply make-product m2)) ;first element is a 1
   ((not (pair? (cdr m2)))
    (if (and (number? m1) (number? (car m2)))
	(* m1 (car m2))
	(list '* m1 (car m2))))
   (else (apply make-product (make-product m1 (car m2)) (cdr m2)))))

(define (product? x)
   (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p)) 

(define (multiplicand p) (apply make-product (cddr p)))
