(define (negate scheme-number)
  (- scheme-number))

(define (negate rational)
  (make-rat (- (numer rational)) (denom rational)))

(define (negate complex)
  (make-complex-from-real-image (- (real-part complex)) (- (imag-part complex))))


(define (install-polynomial-package)
  (put 'negate '(polynomial)
       (lambda (x)
	 (negate-poly x)))
  (define (negate-poly poly)
    (if (empty-termlist? poly)
	the-empty-termlist
	((let (t1 (first-term poly)))
	 (adjoin-term
	  (make-term (order t1) (negate (coeff t1)))
	  (negate-poly (rest-terms poly))))))
  
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (define (sub-poly L1 L2)
    (add-poly L1 (negate L2))))
