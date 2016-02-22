(define (each? exp) (tagged-list? 'each))
(define (each-args exp) (cdr exp))
(define (each-list args) (car args))
(define (each-proc args) (cadr args))

(define (each->seq exp)
  (expand-each (each-args exp)))

(define (expand-each args)
  (sequence->exp
   ;;lots of lambdas
   (apply-each (each-list args) (each-proc args))
   (each-list args)))

(define (apply-each lst 
