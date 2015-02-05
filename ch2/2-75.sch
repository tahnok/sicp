(define (make-from-mag-ang magnitude angle)
  (define (dispatch op)
    (cond
     ((eq? op 'magnitue) magnitude)
     ((eq? op 'angle) angle)
     ((eq? op 'real-part) (* magnitude (cos angle)))
     ((eq? op 'imaginary-part) (* magnitude (sin angle)))
     (else
      (error "Unknown op" op))))
  dispatch)
