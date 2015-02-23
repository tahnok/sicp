(define (int->rat x)
  ((get 'make 'rational) x 1))

(define (rat->real x)
  ((get 'make-from-real 'complex) (
