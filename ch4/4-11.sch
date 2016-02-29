(define (make-frame variables values)
  (let ((definition (list (cons (car variables) (car values)))))
    (if (null? (cdr variables))
	definition
	(append definition (make-frame (cdr variables) (cdr values))))))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame)) ;assume (cons var val) not (list var val)

(define (add-binding-to-frame! var val frame)
  (set! frame (append (list (cons var val)) frame)))


(define (extend-environment vars vals base-env) ;; same as before
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" var vals)
	  (error "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vars))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))
	     
