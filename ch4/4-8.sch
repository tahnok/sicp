(define (let? exp) (tagged-list? exp 'let))
(define (let-args exp) (cdr exp))
(define (definitions args) (car args))
(define (body args) (cadr args))
(define (vars definitions) (map car defintions))
(define (bodies definitions) (map cadr definitions))

(define (named-let? args) (symbol? (car args)))
(define (named-let-label args) (car args))
(define (named-let-bindings args) (cadr args))
(define (named-let-body args) (caddr args))

(define (let->combination exp)
  (let ((args (let-args exp)))
    (if (named-let? args)
	(expand-named-let args)
	(expand-let-clauses args))))

(define (expand-named-let args)
  (sequence->exp
   (list 'define
	 (cons (named-let-label args)
	       (vars (named-let-bindings args)))
	 (named-let-body args))
   (expand-let-clauses (cdr args))))

(define (expand-let-clauses args)
  (eval 
   (make-lambda (vars (definitions args))
		(body args))
   (bodies (defintions args))))
