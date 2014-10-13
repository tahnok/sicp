(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (rotate-90 (rotate-90 painter)))

(define (rotate-270 painter)
  (rotate-90 (rotate-90 (rotate-90 painter))))
