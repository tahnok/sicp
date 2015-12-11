(define (merge-weighted s t weight-fn)
  (cond ((stream-null? s) t)
	((stream-null? t) s)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (let ((s1weight (weight-fn s1car))
		 (s2weight (weight-fn s2car)))
	     (cond ((< s1weight s2weight)
		    (cons-stream
		     s1car
		     (merge (stream-cdr s1) s2)))
		   ((> s1weight s2weight)
		    (cons-stream
		     s2car
		     (merge s1 (stream-cdr s2))))
		   (else
		    (cons-stream
		     s1car
		     (merge (stream-cdr s1)
			    (stream-cdr s2))))))))))


;;; a

(merge
