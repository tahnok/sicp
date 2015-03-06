(define (project x) (apply-generic 'project x))

(define (install-scheme-number)
  (put 'project '(scheme-number)
       (lambda (x)
	 x)))

(define (install-rational-number)
  (put 'project '(rational)
       (lambda (x)
	 (make-int (/ (numer x) (denom x))))))

(define (install-real)
  (put 'project '(real)
       (lambda (x)
	 (apply make-rat (rationalize x))))) ;rationalize is an imaginary function that takes a real number and attempts to reduce it to a real. It's not clear how we would store a real like pi though, so...

(define (install-complex-number)
  (put 'project '(complex)
       (lambda (x)
	 (make-real (real-part x)))))


(define (drop x)
  (if
   (equ? (project (raise x)) x)
   (drop (project x))
   x))

;now we can simple drop one arg, see if it's possible to raise the other args to it. If not we iterate again and see how low the next arg can be dropped &tc
