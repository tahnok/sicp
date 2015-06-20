(define (count-pairs maybe-pair)
  (let ((seen ()))
    (define (count-pairs-inner maybe-pair)
      (if (or (not (pair? maybe-pair)) (memq maybe-pair seen))
	  0
	  (begin
	    (set! seen (cons seen maybe-pair))
	    (+
	     (count-pairs (car maybe-pair))
	     (count-pairs (cdr maybe-pair))
	     1))))
    (count-pairs-inner maybe-pair)))
