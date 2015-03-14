(define (install-polynomial-package)
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (define (sub-poly L1 L2)
    (cond
     ((empty-termlist? L2) L1)
