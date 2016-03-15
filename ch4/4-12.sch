;; not mine
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

;; mine
(define (lookup-variable-value var env)
  (define scan (scan-builder (lambda () (env-loop (enclosing-environment env)))
				(lambda (vals) (car vals))
				var))
				
  (define env-loop (env-loop-builder scan))
  (env-loop env))

(define (set-variable-value! var val env)
  (define scan (scan-builder (lambda () (env-loop (enclosing-environment env)))
			      (lambda (vals) (set-car! vals val))
			      var))
  (define env-loop (env-loop-builder scan))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    ((scan-builder (lambda () (add-binding-to-frame! var val frame))
		   (lambda (vals) (set-car! vals val))
		   var)
     (frame-variables frame)
     (frame-values frame))))

(define (scan-builder null-proc eq-proc var)
  (define (scan vars vals)
    (cond ((null? vars)
	   (null-proc))
	  ((eq? var (car vars))
	   (eq-proc vals))
	  (else (scan (cdr vars) (cdr vals)))))
  scan)
	   
(define (env-loop-builder scan)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Empty env!" env)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  env-loop)
