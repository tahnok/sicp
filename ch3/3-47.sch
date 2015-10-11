(define (make-semaphore-mutex size)
  (let ((mutexes (mutex-list size)))
    (define (the-semaphore m)
      ((cond ((eq? m 'acquire)
					;try and acquire mutex
	      )
	     ((eq? m 'release)
					;release a mutex?
	      ))))
    the-mutex))

(define (mutex-list size)
  (if (= size 1)
      (list (make-mutex))
      (append (list (make-mutex)) (mutex-list (- size 1)))))
