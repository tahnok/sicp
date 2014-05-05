;v1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< 0 (* n d))
	(cons (abs (/ n g)) (abs (/ d g)))
	(cons (/ (- (abs n)) g) (abs (/ d g))))))
;v2
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
	(cons (/ n g) (/ d g))))
