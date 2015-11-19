(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define (partial-sums stream)
  (cons-stream (stream-car stream) (add-stream (stream-cdr stream) (partial-sums stream))))


(define (ln-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln-summands (+ n 1)))))

(define ln-2-stream
  (partial-sums (ln-summands 1)))

;; after 100 entries, the value is correct to 4 decimal places, so pretty slowly

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

;; with the accelerated-sequence from above, we are correct to 7 decimal places in 4 terms... and after 9 we hit the limit for int accuracy
