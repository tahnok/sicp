(define (letrec? exp) (tagged-list? 'letrec exp))

(define (letrec-args exp) (cdr exp))

(define (letrc-defs args) (car args))
(define (letrec-body args) (cadr args))
(define (letrec-vars defs) (map def car))

(define (letrec->let exp)
  (let ((args (letrec-args exp)))
    (let ((defs (letrec-defs args))
	  (body (letrec-body args)))
      (list 'let
	    (letrec-lets (letrec-vars defs))
	    (make-begin (list (letrec-secs defs) (letrec-body args)))))))

(define (letrec-lets vars)
  (map vars (lambda (var) (cons var '*unassigned))))

(define (letrec-sets defs)
  (map defs (lambda (def) (list 'set! (car def) (cdr def)))))
