(define (merge-weighted s t weight-fn)
  (cond ((stream-null? s) t)
	((stream-null? t) s)
	(else
	 (let ((scar (stream-car s))
	       (tcar (stream-car t)))
	   (let ((sweight (weight-fn scar))
		 (tweight (weight-fn tcar)))
	     (cond ((< sweight tweight)
		    (cons-stream
		     scar
		     (merge (stream-cdr s) t)))
		   ((> sweight tweight)
		    (cons-stream
		     tcar
		     (merge s (stream-cdr t))))
		   (else
		    (cons-stream
		     scar
		     (merge (stream-cdr s)
			    (stream-cdr t))))))))))

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
		       (expt (cdr pair) 3)))))

(define (repeated-pairs stream)
  (
