;; not mine

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers
  (integers-starting-from 1))

;; mine

(define (merge-weighted s1 s2 weight-fn)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (let ((s1weight (weight-fn s1car))
		 (s2weight (weight-fn s2car)))
	     (cond ((< s1weight s2weight)
		    (cons-stream
		     s1car
		     (merge-weighted (stream-cdr s1) s2 weight-fn)))
		   ((> s1weight s2weight)
		    (cons-stream
		     s2car
		     (merge-weighted s1 (stream-cdr s2) weight-fn)))
		   (else
		    (cons-stream
		     s1car
		     (merge-weighted
		      (stream-cdr s1)
		      s2
		      weight-fn)))))))))


(define (weighted-pairs s t weight-fn)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s)
		    (stream-cdr t)
		    weight-fn)
    weight-fn)))

;;
(define (square-sum x) (+ (expt (car x) 2)
			  (expt (cadr x) 2)))

(define squares
  (weighted-pairs integers integers  square-sum))
	
(define (repeated-triples stream comparator)
  (if (= (comparator (stream-car stream))
	 (comparator (stream-car (stream-cdr stream)))
	 (comparator (stream-car (stream-cdr (stream-cdr stream)))))
      (cons-stream
       (list
	(comparator (stream-car stream))
	(stream-car stream)
	(stream-car (stream-cdr stream))
	(stream-car (stream-cdr (stream-cdr stream))))
       (repeated-triples (stream-cdr stream) comparator))
      (repeated-triples (stream-cdr stream) comparator)))

(define answer (repeated-triples squares square-sum))
