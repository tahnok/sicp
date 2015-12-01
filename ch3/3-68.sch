;; louis' code

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

;; causes an error about maxium recursion depth exceeded...
;; why?
