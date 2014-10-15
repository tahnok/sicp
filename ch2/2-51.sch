(define (below painter1 painter 2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((pain-top
	   (transform-painter
	    painter2
	    split-point
	    (make-vect 1 0.5)
	    (make-vect 0 1)))
	  (paint-bot
	   (tranform-painter
	    painter1
	    (make-vect 0 0)
	    split-point
	    (make-vect 0 1))))
      (lambda (frame)
	(paint-top frame)
	(paint-bot frame)))))
