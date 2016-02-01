;; original 
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; always left to right

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((head (eval (first-operands exps env))))
	(let ((tail (list-of-values (rest-operands exps) env)))
	  (cons head tail)))))

;with set!
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (begin
	(set! tail (list-of-values (rest-operands exps) env))
	(set! head (eval (first-operands exps env)))
	(cons head tail))))
