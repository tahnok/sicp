;; not mine
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;;mine
(define (make-frame variables values)
  (let ((definition (list (cons (car variables) (car values)))))
    (if (null? (cdr variables))
	definition
	(append definition (make-frame (cdr variables) (cdr values))))))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame)) ;assume (cons var val) not (list var val)

(define (add-binding-to-frame! var val frame)
  (if (null? (cdr frame))
      (set-cdr! frame (cons (cons var val) (list)))
      (add-binding-to-frame! var val (cdr frame))))

(define (extend-environment vars vals base-env) ;; same as before
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" var vals)
	  (error "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env) ;; works as before
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))
	     
;;no error, but not working
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (caar frame))
	     (set-car! (car frame) var)
	    (else (scan (cdr frame))))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(scan (first-frame env))))
    (env-loop env))
	     
