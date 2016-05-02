;; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (let ((val (car vals)))
	       (if (eq? val '*unassigned*)
		   (error "UNASSIGNED variable" var)
		   val)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; b

(define (scan-out-defines proc-body)
  (let ((extractions (extract proc-body '() '())))
    (
  )

(define (extract sequence defintions rest)
  (if (null? sequence)
      (cons defintions rest)
      (let ((exp (car sequence))
	    (rest (cdr sequence)))
	(if (defintion? exp)
	    (extract rest (append defitions (list exp)) rest)
	    (extract rest defintions (append rest (list exp)))))))