;; not mine

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers
  (integers-starting-from 1))

;; mine

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
		      (stream-cdr s2)
		      weight-fn)))))))))


(define (weighted-pairs s t weight-fn)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (append (list stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s)
		    (stream-cdr t)
		    weight-fn)
    weight-fn)))

;;

(define cubes
  (weighted-pairs integers
		  integers
		  (lambda (pair)
		    (+ (expt (car pair) 3)
		       (expt (cadr pair) 3)))))

(define (repeated-pairs stream comparator)
  (if (= (comparator (stream-car stream))
	 (comparator (stream-car (stream-cdr stream))))
      (cons-stream
       (cons (stream-car stream) (stream-car (stream-cdr stream)))
       (repeated-pairs (stream-cdr stream) comparator))
      (repeated-pairs (stream-cdr stream) comparator)))
