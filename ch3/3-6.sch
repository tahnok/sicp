(define (random-init)
  4)

(define (rand message)
  (let ((x random-init))
    (define (generate)
      (begin
	(set! x (rand-update x))
	x))
    (define (reset val)
	(set! x val))
    (define (dispatch message)
      (cond
       ((eq? message 'generate)
	(generate))
       ((eq? message 'reset)
	reset)
       (else
	(error "pew"))))
    (dispatch message)))
