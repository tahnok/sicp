(define (random-in-range low high)
  (let ((range (- high low)))
  (+ low (random range)))) 
