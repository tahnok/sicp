(define (subtraction a b)
  (make-interval
   (min (- (lower-bound a) (upper-bound b)) (- (lower-bound b) (upper-bound a)))
   (max (- (upper-bound b) (lower-bound a)) (- (upper-bound a) (lower-bound b)))))
