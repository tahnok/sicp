;; not mine

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral integrand initial-value dt)
  (define int
	   (cons-stream initial-value
			(add-streams (scale-stream integrand dt)
				     int)))
  int)

;; mine

(define (RC R C dt)
  ;;todo
  (define (proc initial-volts current-stream)
    (cons-stream
     initial-volts
     (add-streams
      (scale-stream current-stream R)
      (integral
       (scale-stream
	current-stream
	(/ 1 C))
	initial-volts
	dt))))
  proc)
