(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

;; this destructively reverses the list, return x backwards, and leaving x set to just the last element
;; note: the returned list still points to x, so changes to x will be reflected in the newly returned list
