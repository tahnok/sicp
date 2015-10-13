(define (make-semaphore-mutex size)
  (let ((free-mutexes (mutex-list size))
	(taken-mutexes (list)))
    (define (aquire-first)
      (if (null? mutex-list)
	  #f
	  (let ((first-mutex (car free-mutexes)))
	    (if (first-mutex 'acquire)
		(begin
		  (set-car! free-mutexes (cdr free-mutexes))
		  (set-car! 
		  )
		(acquire-first)))))
    (define (release-first mutex-list)
      (if (not (null? mutex-list))
	  (if ((car mutex-list)))))
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
