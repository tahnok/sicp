(define (user-print object)
  (cond ((compound-procedure? object)
	 (display (list 'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<proceduer-env>)))
	((lazy-stream? object)
	 (display (append
		   (list 'lazy-stream)
		   (force-n object 4))))
	(else (display object))))

(define (force-n stream n)
  (if (cdr stream)
      (if (= n 0)
	  '(...)
	  (append
	   (list (car stream)
		 (force-n (cd stream) (- n 1)))))
      '()))

(define (lazy-stream object)
  (tagged-list? object 'cons))
      
