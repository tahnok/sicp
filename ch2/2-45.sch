(define (split parent child)
  (lambda (painter n)
    (if (= n 1)
	painter
	(let ((smaller ((split parent child) painter (- n 1))))
	  (parent painter (child smaller smaller))))))
    
