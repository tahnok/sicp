(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number)
					;assume existing methods here
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y))))

(define (install-rational-number)
  (put 'equ? '(rational rational)
       (lambda (x y)
	 (and (= (numer x) (numer y)) (= (denom x) (denom y))))))

(define (install-complex-package)
  (put 'equ? '(rational rational)
       (lambda (x y)
	 (and (= (real-part x) (real-part y))
	      (= (imag-part x) (image-part y))))))
