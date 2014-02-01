;;;;;;;;;;;;; 1.1
10
;10
(+ 5 3 4)
;12
(- 9 1)
;8
(/ 6 2)
;3
(+ (* 2 4) (- 4 6))
;6
(define a 3)
;a (3)
(define b (+ a 1))
;b (4)
(+ a b (* a b))
;19
(= a b)
;false
(if (and (> b a) (< b (* a b)))
    b
    a)
; b
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;16
(+ 2 (if (> b a) b a))
;6 
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;16

;;;;;;;;;;;;;;; 1.2

(/ (+ 4 5 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;;;;;;;;;;;;;; 1.3

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-of-larger-square x y z)
  (if (> x y)
      (sum-of-squares x (if (> y z) y z))
      (sum-of-squares y (if (> x z) x z))
))

;;;;;;;;;;;;;;;; 1.4

;code:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;we use if to decide if we should add b (if it's positive) or subtract b (if it's negative) by using if to return an operator

;;;;;;;;;;;;;;; 1.5

;question code:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))


;If applicative order evaluation is used (where we evaluate the arguments and then apply) we will see an error related to infinite recursion since p calls p forever. If Normal order evaluation is used (fully expand then reduce) it will print zero

;;;;;;;;; 1.6

;code:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;;This results in a maxiumum recursion depth exceeded error because of the order of evaluation. Scheme will try to apply evaluate then and else clause leading to an infinite recursion.

;;;;;;;;; 1.7

;sample code:

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y
  (/ (+ x y) 2)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;my code

(define (square-iter2 current-guess last-guess x)
  (if (good-enough2? current-guess last-guess)
      current-guess
      (square-iter2 (improve current-guess x) current-guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough2? current-guess last-guess)
  (< (/ (abs (- current-guess last-guess)) current-guess)
     (/ current-guess 1000)))

(define (sqrt x)
  (square-iter2 1.0 0.0 x))


;1.8

(define (cube-iter current-guess last-guess x)
  (if (good-enough2? current-guess last-guess)
      current-guess
      (cube-iter (cube-improve current-guess x) current-guess x)))

(define (cube-improve x y)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (cube x)
  (cube-iter 1.0 2.0 x))

;1.10

;;code

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;1.11

;; recursive

(define (f n)
  (cond ((< n 3) n)
	(else (+
	       (f (- n 1))
	       (* 2 (f (- n 2)))
	       (* 3 (f (- n 3)))
	       ))))

;; iterative

(define (f-iter n x a b c)
  (cond ((< n 3) n)
	((= n x) a)
	(else (f-iter n (+ x 1) (+ a (* 2 b) (* 3 c)) a b))))

(define (f2 n)
  (f-iter n 2 2 1 0))

;1.12 pascal's triangle

(define (pascal x y)
  (cond	((= x 0) 0)
	((= x 1) 1)
	((> x y) 0)
	(else (+ (pascal x (- y 1)) (pascal (- x 1) (- y 1))))))

;1.16 Iterative fast exponentiation

(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

(define (exp-iter b n a)
  (cond ((= n 0) a)
	((even? n) (exp-iter (square b) (/ n 2) a))
	(else (exp-iter b (- n 1) (* a b)))))

(define (fast-exp b n)
  (exp-iter b n 1))

;1.17 iterative fast multiplication

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multip-iter a b c)
  (cond ((= a 0) c)
	(else (multip-iter (- a 1) b (+ c b)))))

(define (multip a b)
  (multip-iter a b 0))

(define (fast-multip a b)
  (cond ((= a 1) b)
	((even? a) (fast-multip (halve a) (double b)))
	(else (+ b (fast-multip (- a 1) b)))))

;1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	(fib-iter
	 a
	 b
	 (+ (* q q) (* p p))
	 (+ (* 2 p q) (* q q))
	 (/ count 2)))
  (else (fib-iter (+ (* b q) (* a q) (* a p))
		  (+ (* b p) (* a q))
		  p
		  q
		  (- count 1)))))
