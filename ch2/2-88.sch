(define (negate scheme-number)
  (- scheme-number))

(define (negate rational)
  (make-rat (- (numer rational)) (denom rational)))

(define (negate complex)
  (make-complex-from-real-image (- (real-part complex)) (- (imag-part complex))))


(define (install-polynomial-package)
  (put 'negate '(polynomial)
       (;TODO CODE HERE
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (define (sub-poly L1 L2)
    (cond
     ((empty-termlist? L2) L1)
     ((empty-termlist? L1) (negate L2))
     (else
      (let ((t1 (first-term L1))
	    (t2 (first-term L2)))
	(cond ((> (order t1) (order t2))
	       (adjoin-term
		(negate t1) (sub-poly (rest-terms L2) L1)))
	      ((< (order t1) (order t2)
		  (adjoin-term
		   (negate t2) (sub-poly L1 (rest-terms L2)))))
	      (else
	       (adjoin-term
		(make-term
		 (sub (coef t1) (coef t2)))
		(sub-poly (rest-terms L1) (rest-terms L2))))))))))
