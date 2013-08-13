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

