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

(define (weighted-pairs s t weight-fn)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (append (list stream-car s) x))
		(stream-cdr (t)))
    (weighted-pairs (stream-cdr s)
		    (stream-cdr t)
		    weight-fn)
    weight-fn)))

;;; a

(weighted-pairs integers
		integers
		(lambda (pair)
		  (+ (car pair) (cdr pair))))

;;; b

(define stream235
  (stream-filter
   (lambda (item) (and
		   (= (remainder item 2) 0)
		   (= (remainder item 3) 0)
		   (= (remainder item 5) 0)))
   integers))

(weighted-pairs stream235
		stream235
		(lambda (pair)
		  (+
		   (* 2 (car pair))
		   (* 3 (cdr pair))
		   (* 5 (car pair) (cdr pair)))))
