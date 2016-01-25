(define (random-stream input-stream)
  (define (rand-stream input-stream last)
    (if (= 'reset (car (stream-car input-stream)))
	(cons-stream
	 'reset
	 (rand-stream (stream-cdr input-stream) (cadr (stream-car input-stream))))
	(let ((new (rand-update last)))
	  (cons-stream
	   new
	   (rand-stream (stream-cdr input-stream) new)))))
  (rand-stream input-stream 0))
