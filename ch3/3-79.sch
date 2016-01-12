(define (solve-2nd f y0 dy0 dt)
  (define y (intgral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt)) ; not sure
  (define ddy (stream-map f dy y))
  y)
  
