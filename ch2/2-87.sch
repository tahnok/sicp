(define (install-polynomial-package)
  (define (zero-terms term)
    (if
     (empty-term-list? x)
     #t
     (and
      (=zero? (ceoff first-term x)))
      (zero-terms? (rest-terms x))))
  (put '=zero? 'polynomial (lambda (x)
			     (zero-terms (term-list x))))
  )
