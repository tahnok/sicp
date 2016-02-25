(define (each? exp) (tagged-list? 'each))
(define (each-args exp) (cdr exp))
(define (each-list args) (car args))
(define (each-proc args) (cadr args))

(define (each->seq exp)
  (expand-each (each-args exp)))

(define (expand-each args)
  (sequence->exp
   (list
    (apply-each (each-list args) (each-proc args))
    (each-list args)))

(define (apply-each list proc)
  (if (not (empty? list))
      (append (list (apply proc (car list)))
	      (apply-each (cdr list) proc))
      '()))


(define (while? exp) (tagged-list? 'while))
(define (while-args exp) (cdr exp))
(define (while-cond args) (car args))
(define (while-body args) (cadr args))

(define (while->seq exp)
  (let ((args (while-args exp)))
    (make-if (while-cond args)
	     (sequence-exp (list (eval (while-body args))
				 exp))
	     'done)))


