;; a)

;; louis' idea won't work because application? returns true if the expression is a pair,
;; which means it would match defines, assignmnts, ifs, conds and lambdas
;; eg: evaluating (define x 3) would assume that define is a procedure to apply,
;; then blow up when it tried to resolve x to a variable!


;; b)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond-if exp) env))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (application? exp) (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (caddr exp exp))

(define (no-operands? ops) (null? ops))

(define (first-operands ops) (car exp))

(define (rest-operands ops) (cdr ops))
