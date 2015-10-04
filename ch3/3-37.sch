;;; example

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;;; mine

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* a b)
  (let ((d (make-connector)))
    (multiplier a b d)
    d))

(define (c/ a b)
  (let ((c (make-connector)))
    (multiplier b c a))
  c)

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    x))
