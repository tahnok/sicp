(define (raise x) (apply-generic 'raise x))

; in integer / scheme-number package
(put 'raise '(scheme-number)
     (lambda (x)
       (make-rational (contents x) 1)))

;in rational
(put 'raise '(rational)
     (lambda (x)
       (make-real (/ (numer x) (denom y)))))

;in real
(put 'raise '(real)
     (lambda (x)
       (make-complex-from-real-imag x 0)))
