(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (count 2 z))

(define (cdr z)
  (count 3 z))

(define (count n base)
  (if (= (modulo n base) 0)
      (+ 1 (count (/ n base) base))
      0))
