(define (require p)
  (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; mine

(define (an-integer-between n m)
  (if (> n m)
      (amb)
      (amb n (an-integer-between (+ n 1) m))))
