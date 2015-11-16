(define (div-series a b)
  (if (eq? (car-stream b) 0)
      (error "can't divide stream if constant term of b is zero")
      (mul-stream a (invert-unit-series b))))

(define tan-series
  (div-series (sine-series) (cos-series)))


  
