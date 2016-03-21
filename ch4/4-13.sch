;; questions
;; should unbind only first frame or any frame?
;; should handle already unbound vars how?
;; procedures are bound... does it work on them?
;;
;; this procedure will only unbind in the current frame.
;; Other calls may depend on things being bound in other frames.
;; Unbinding in the current frame should be sufficient for any cases where this is necessary
;;
;; It will throw an exception if you attempt to unbind a variable that is not bound
;;
;; procedures are not addressed here, only variables
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (error "Unbound variable --make-unbound!" var))
	    ((eq? var (car vars))
	     (remove-binding-from-frame! var frame))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variable frame)
	  (frame-values frame))))

(define (remove-binding-from-frame! var frame)
  (define (scan vars vals)
    (if (eq? (cadr vars) var)
	(begin
	  (set-cdr! vars (cddr vars))
	  (set-cdr! vals (cddr vals)))
	(scan (cdr vars) (cdr vals))))
  (let ((vars (frame-variables frame))
	(vals (frame-values frame)))
    (if (eq? (car vars) var)
	(begin
	  (set-car! vars (cadr vars))
	  (set-cdr! vars (cddr vars))
	  (set-car! vals (cadr vals))
	  (set-cdr! vals (cddr vals)))
	(scan vars vals))))
;;TODO: make work with frame with only one var/val pair
