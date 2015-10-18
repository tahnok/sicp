(define (make-semaphore-mutex size)
   (define (mutex-list size)
     (if (= size 1)
	 (list (make-mutex))
	 (append (list (make-mutex)) (mutex-list (- size 1)))))
   (let (mutexes (cons
		  (mutex-list size)
		  (taken-mutexes (list))))
    (define (aquire-first)
      (if (null? (car mutexes))
	  #f
	  (let ((first-mutex (caar mutexes)))
	    (if (first-mutex 'acquire)
		(begin
		  (set-car! mutexes (cdar mutexes))
		  (set-cdr! mutexes (append (cdr mutexes) (list first-mutex)))
		  #t)
		(acquire-first))))) ;try again
		
    (define (release-first)
      (if (null? (cdr mutexes))
	  #f
	  (let ((first-mutex (cadr mutexes)))
	    (if (first-mutex 'release)
		(begin
		  (set-car! mutexes (append (car mutexes) (list first-mutex)))
		  (set-cdr! mutexes (cddr mutexes)))
		(release-first)))))
    (define (release-first mutex-list)
      (if (not (null? mutex-list))
	  (if ((car mutex-list)))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	      (acquire-first))		;try and acquire mutex
	     ((eq? m 'release)
	      (relase-first) 		;release a mutex?
	      )))
    the-mutex))

(define (make-test-and-set-mutex size)
  (define (make-semaphore size)
    (if (= size 1)
	(list (list false))
	(append (make-mutex (- size 1)) (list (list false)))))
  (define (acquire-fist semaphore)
    (if (null? semaphore)
	(error "can't acquire lock")
	(if (test-and-set! (car semaphore))
	    (acquire-first (cdr semaphore))
	    true)))

  (define (release-last semaphore)
    (if (caar semaphore)
	(if (or
	     (null? cdr semaphore)
	     (not (caadr semaphore)))
	    (set-car! (car semaphore) false)
	    (release-liast (cdr semaphore)))
	(error "?????")))
	    

  (let ((semaphore (make-semaphore size)))
    (define (the-semaphore m)
      (cond
       ((eq? m 'acquire)
	(acquire-first semaphore))
       ((eq? m 'release)
	(release-last))
       (else
	(error "unknown command"))))
    the-semaphore))
