(define (RLC R L C dt)
  (define (thang vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define dvc (scale-stream il (/ 1 (- C))))
    (define il (integral (delay dil) il0 dt))
    (define dil (add-streams
		 (scale-stream il (/ (- R) L))
		 (scale-stream vc (/ 1 L))))
    (stream-map (lambda (vc il)
		  (list (stream-car vc) (stream-car il)))
		vc
		il))
  thang)
