(define
  (=zero? x ) (apply-generic '=zero? x))

(define (install-scheme-number)
  (define (=zero? x)
    (= x 0))
  (put '=zero? '(scheme-number) =zero?))

(define (install-rational-number)
  (define (=zero? x)
    (= (number x) 0))

  (put '=zero? '(rational) =zero?))

(define (install-complex-package)
  (define (=zero? x)
    (and (= (real-part x) 0) (= (imag-part x) 0)))

  (put '=zero? '(complex) =zero?))
