(define (rand-old (let ((x random-init))
		(lambda ()
		  (set! x (rand-update x))
		  x))))

(define (rand message)

  (define (dispatch)
    (cond
     ((= message 'generate)
      (rand-generate))
     ((= message 'reset)
      (lambda (x)
	(set! poop x)))
     (else
      (error "pew"))))
  (dispatch message))
