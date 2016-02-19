(define (let? exp) (tagged-list? exp 'let))
(define (let-args exp) (cdr exp))
(define (definitions args) (car args))
(define (body args) (cadr args))
(define (vars definitions) (map car defintions))
(define (bodies definitions) (map cadr definitions))

(define (let->combination exp)
  (expand-let-clauses (let-args exp)))

(define (expand-let-clauses args)
  (eval 
   (make-lambda (vars (definitions args))
		(body args))
   (bodies (defintions args))))
