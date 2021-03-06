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

;1.21

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199) ;value 199
(smallest-divisor 1999) ;value 1999
(smallest-divisor 199) ;value 7

(define (prime? n)
  (= n (smallest-divisor n)))

;1.22

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (<= start end) (timed-prime-test start))
  (if (<= start end)
      (cond ((= (remainder start 2) 0)
	    (search-for-primes (+ 1 start) end))
	    (else (search-for-primes (+ start 2) end)))))

(search-for-primes 1000 1100)
;1009
;1013
;1019

(search-for-primes 10000 10100)
;10007
;10009
;10037

(search-for-primes 100000 100100)
;100003
;100019
;100043

(search-for-primes 1000000 1000100)
;1000003
;1000033
;1000037

(search-for-primes 100000000 100000100)
;100000007 *** .01999999999999999
;100000037 *** 2.0000000000000018e-2
;100000039 *** 9.999999999999981e-3
;100000049 *** 2.0000000000000018e-2
;100000073 *** 2.0000000000000018e-2
;100000081 *** 9.999999999999953e-3

(search-for-primes 1000000000 1000000100)
;1000000007 *** 5.0000000000000044e-2
;1000000009 *** .04999999999999982
;1000000021 *** 5.0000000000000044e-2
;1000000033 *** .06000000000000005
;1000000087 *** .04999999999999982
;1000000093 *** 5.0000000000000044e-2
;1000000097 *** 5.0000000000000044e-2

(search-for-primes 10000000000 10000000100)
;10000000019 *** .16000000000000014
;10000000033 *** .16000000000000014
;10000000061 *** .1599999999999997
;10000000069 *** .16000000000000014
;10000000097 *** .16000000000000014

;1.23

(define (next n)
  (cond ((= n 2) 3)
	(else (+ 2 n))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (<= start end) (timed-prime-test start))
  (if (<= start end)
      (cond ((= (remainder start 2) 0)
	    (search-for-primes (+ 1 start) end))
	    (else (search-for-primes (+ start 2) end)))))

(search-for-primes 10000000000 10000000100)
;10000000019 *** .0900000000000003
;10000000033 *** .09999999999999964
;10000000061 *** .0900000000000003
;10000000069 *** .10000000000000009
;10000000097 *** .10000000000000009

;this test should be twice as fast since half as many values are checked now, but it is only 1.6 times faster. It's possible that the extra if is introducing another instruciton per check

;1.24

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? np times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (<= start end) (timed-prime-test start))
  (if (<= start end)
      (cond ((= (remainder start 2) 0)
	    (search-for-primes (+ 1 start) end))
	    (else (search-for-primes (+ start 2) end)))))

(search-for-primes 10000000000 10000000100)
;10000000019 *** 1.0000000000000009e-2
;10000000033 *** 1.9999999999999962e-2
;10000000061 *** 1.0000000000000009e-2
;10000000069 *** 1.0000000000000009e-2
;10000000097 *** 1.0000000000000009e-2

(search-for-primes 100000000000 100000000100)
(search-for-primes 1000000000000 1000000000100)
(search-for-primes 10000000000000 10000000000100)
(search-for-primes 100000000000000 100000000000100)

;1.25
;She is technically correct, but this solution will computer very large numbers and result in much slower run times

;1.26
;The intrerpreter will evalutate (remainder (* (expmod base (/exp 2) m) (expmod base (/ exp 2) m)) m) such that expmod is called twice instead of just once and then doubling the result. But since this is a recursive procedure, we go into tree recursion calling twice the expmod at every level

;1.27

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (exhaustive-fermat testing base)
  (cond ((<= testing base) true)
	((= (expmod base testing testing) base) (exhaustive-fermat testing (+ base 1)))
	(else false)))

(exhaustive-fermat 32 1) ;f
(exhaustive-fermat 7 1) ;t (true prime)
;carmichael numbers
(exhaustive-fermat 561 1) ;t
(exhaustive-fermat 1105 1) ;t
(exhaustive-fermat 1729 1) ;t
(exhaustive-fermat 2465 1) ;t
(exhaustive-fermat 2821 1) ;t
(exhaustive-fermat 6601 1) ;t

;1.28

(define (square x) (* x x))

(define (expmod base exp m)
  (define (non-trivial-root? intermediate)
    (define (check-result result)
      (if (and
	   (not (= intermediate (- m 1)))
	   (not (= intermediate 1))
	   (= result))
	  0
	  result))
    (check-result (remainder (square intermediate) m)))

  (cond ((= exp 0) 1)
	((even? exp)
	 (non-trivial-root? (expmod base (/ exp 2) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m)))))

(define (miller-rabin testing)
  (define (miller-rabin-iter testing count)
    (cond ((<= (- testing 1) count) true)
	  ((= (expmod count (- testing 1) testing) 0) (display count))
	  (else (miller-rabin-iter testing (+ count 1)))))
  (miller-rabin-iter testing 3))

;1.28 --------------------------- take 2

(define (square x) (* x x))

(define (expmod base exp m)
  (define (check-result result step exp)
    (if (and (= result 1) (not (= step 1)) (not (= step (- exp 1))))
	0
	result
	))
  (define (make-step step exp m)
    (check-result (remainder (square step) m) step exp))
  (cond ((= exp 0) 1)
	((even? exp)
	 (make-step (expmod base (/ exp 2) m) exp m)
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (exhaustive-fermat testing base)
  (cond ((<= testing base) true)
	((= (expmod base testing testing) base) (exhaustive-fermat testing (+ base 1)))
	(else false)))

;1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (incr n) (+ 1 n))

(define (simpson-integral f a b n)
  (define (get-h) (/ (- b a) n))
  (define (prefix current)
    (cond ((= current 0) 1)
	  ((= current n) 1)
	  ((even? current) 2)
	  (else 4)))
  (define (term x)
    (* (prefix x) (f (+ a (* x (get-h))))))
  (*
   (sum term 0 incr n)
   (/ (get-h) 3)
   ))

(simpson-integral cube 0 2 1000)

;1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a))))
  (iter a 0))

;1.31

;a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (incrc n) (+ n 1))
(define (ident n) n)

(define (pi count)
  (define (term n)
    (if (even? n)
	(/ n (+ n 1))
	(/ (+ n 1) n)))
  (product term 2 incr count))

;b

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

;1.32

;a

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;b

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

;1.33

(define (accumulate-filter combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (if (filter a)
			   (combiner result (term a))
			   (result)))))
  (iter a null-value))

;a

(define (sum-square-prime a b)
  (define (square x) (* x x))
  (accumulate-filter + 0 square a incr b prime?))

;b

(define (sum-relative-prime a)
  (define (relative-prime? x)
    (= 1 (gcd x a))
  (accumulate-filter * 1 ident 1 incr a relative-prime?))

;1.34

;it would call (2 2) and fail since you can't call 2

;1.35

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;p^2 = p + 1

;p = p^2 - 1

;1 = p - 1/p

;p = 1 + 1 / p

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))

;1.36

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-to-x)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1))

(define (x-to-x-average)
  (define (average x y)
    (/ (+ x y) 2))
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.1))

;far fewer iterations for x-to-x-average

;1.37

;a

(define (cont-frac n d k)
  (define (a count)
    (if (= k count)
	(/ (n count) (d count))
	(/ (n count) (+ (d count) (a (+ count 1))))))
  (a 1))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)

;b

(define (cont-frac-iter n d k)
  (define (a count sum)
    (if (= 0 count)
	sum
	(a (- count 1) (/ (n count) (+ (d count) sum)))))
  (a k 0))

;1.38

(define (e-approx)
  (+ 2
     (cont-frac-iter (lambda (i) 1.0) (lambda (i) (if
						   (= (remainder (+ i 1) 3) 0)
						   (* 2 (/ (+ i 1) 3))
						   1)) 50)))
;1.39

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if
			       (= i 1)
			       x
			       (- (* x x))))
		  (lambda (i) (- (* 2 i) 1))
		  k))


;1.40

(define (cubic a b c)
  (lambda (x)
    (+
     (* x x x)
     (* a x x)
     (* b x)
     c)))

;1.41

(define (double proc)
  (lambda (x)
    (proc (proc x))))
     
;1.41

(define (combine f g)
  (lambda (x)
    (f (g x))))

;1.43

(define (repeated f n)
  (if
   (= n 1)
   f
   (lambda (x)
     (f ((repeated f (- n 1)) x)))))

(define (square x) (* x x))

((repeated square 2) 5)

;1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

;1.45
(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (repeated f n)
  (if
   (= n 1)
   f
   (lambda (x)
     (f ((repeated f (- n 1)) x)))))

(define (log2 n)
  (/ (log n) (log 2)))

(define (nth-root n radicand)
  (fixed-point ((repeated average-damp (floor (log2 n))) (lambda (y) (/ n (expt y (- radicand 1))))) 1.0))

;1.46

(define (iterative-improve good-enough? improve)
  (define (fn x)
    (let ((xim (improve x)))
      (if (good-enough? xim x) xim (fn xim))))
  fn)

(define (sqrt n)
  (
   (iterative-improve
    (lambda (ximm x) (< (abs (- (square ximm) n)) 0.001))
    (lambda (x) (average x (/ n x))))
   1.0))

(define (fixed-point f guess)
  ((iterative-improve
    (lambda (v1 v2) (< (abs (- v1 v2)) tolerance))
    f)
   guess))


(define tolerance 0.00001)
