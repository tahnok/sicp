;supplied

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;ours

(define (matrix-*-vector m v)
  (map (lambda (x y) (accumulate + 0 (map * x y))) v m))
