(define (combine a b)
  (cons-stream
   (list (stream-car a) (stream-car b))
   (combine (stream-cdr a) (stream-cdr b))))

(define (RLC R L C dt)
  (define (thang vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ 1 (- C))))
    (define dil (add-streams
		 (scale-stream il (/ (- R) L))
		 (scale-stream vc (/ 1 L))))
    (combine vc il)
    )
  thang)
