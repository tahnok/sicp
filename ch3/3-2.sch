(define (make-monitor f)
  (let ((calls 0))
    (lambda (arg . args )
      (cond
       ((eq? arg 'how-many-calls?) calls)
       ((eq? arg 'reset-count) (set! calls 0))
       (else
	(begin
	  (set! calls (+ calls 1))
	  (apply f (append (list arg) args))))))))
