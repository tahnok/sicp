(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

;; This will never return if you use the create cyclic list (eg tail points to the head of the list)

(count-pairs (list 1 2 3));; => 3


(define second (cons 'a 'b))
(define third (cons 'a 'b))
(define first (cons second third))
(set-car! third second)
(count-pairs first) ;; => 4
