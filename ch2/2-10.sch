;When a interval spans zero, the inverse looses the order of the signs. eg: (-2, 2)^-1 = (1/2, -1/2) which is wrong

(define (span-zero interval)
  (and (> (upper-bound interval) 0) (< (lower-bound interval) 0)))

(define (div-interval x y)
  (if (span-zero y)
      (error "ERROR: Y spans zero")
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))
