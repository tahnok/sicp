(define (average stream1 stream2)
  (cons-stream (/ (+ (stream-car stream1) (stream-car stream2)) 2)
	       (average (stream-cdr stream1) (stream-cdr stream2))))

(define (smooth stream)
  (average stream (stream-cdr stream)))

(define (make-zero-crossings input-stream smoothing detector)
  (stream-map
   (lambda (pair) (apply detector pair))
   (each-pair (smoothing input-stream))))

(define (each-pair stream)
  (cons-stream (list (stream-car stream) (stream-car (stream-cdr stream)))
	       (each-pair (stream-cdr))))
