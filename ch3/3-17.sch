(define (count-pairs maybe-pair)
  (let ((seen ()))
    (define (count-pairs-inner maybe-pair)
      (if (not (pair? maybe-pair))
	  0
	  (let (
		(first (car maybe-pair))
		(last (cdr maybe-pair)))
	    (+
	     (if
	      (not (memq first seen))
	      (begin
		;; add car to seen
		(set! seen (append seen (if (list? first) first (list first))))
		(count-pairs-inner first))
	      0)
	     (if
	      (not (memq last seen))
	      (begin
		;; add cdr to seen
		(set! seen (append seen last))
		(count-pairs-inner last)))
	    0)
	   1)))
    (count-pairs-inner maybe-pair)))
