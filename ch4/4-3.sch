;; original

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variale-value exp env))
	((quoted? exp) (text-of-quotation exp))
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
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; data-driven

(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variale-value exp env))
   ((get 'op (car exp)) ((get 'op (car exp)) exp env))
   ((application? exp)
    (apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
   (else
    (error "Unknown expression type -- EVAL" exp))))

(put 'op 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'op 'set! eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if eval-if)
(put 'op 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
(put 'op 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'op 'cond (lambda (exp env) (eval (cond->if exp) env)))
