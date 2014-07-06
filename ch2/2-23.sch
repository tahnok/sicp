(define (for-each proc lst)
  (cond ((null? lst) #t)
	(else
	 (proc (car lst))
	 (for-each proc (cdr lst)))))
	
  
