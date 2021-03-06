(define (make-table)
  (define contains assoc)
  (define (is-table? maybe-table)
    (and (list? maybe-table)
	 (> (length maybe-table) 0)
	 (eq? '*table* (car maybe-table))))

		 
  (define (lookup keys sub-table)
    (let ((record
	   (contains (car keys) (cdr sub-table))))
      (if record
	  (if (eq? '() (cdr keys))
	      (cdr record)
	      (if (is-table? (cdr record))
		  (lookup (cdr keys) (cdr record))
		  false))
	  false)))
  (define (insert! keys value sub-table)
    (let ((record
	   (contains (car keys) (cdr sub-table))))
      (if record
	  (if (eq? '() (cdr keys))
	      (set-cdr! record value)
	      (if (is-table? (cdr record))
		  (insert! (cdr keys) value (cdr record))
		  (let ((new-table (list '*table*)))
		    (insert! (cdr keys) value new-table)
		    (set-cdr! sub-table
			      (cons
			       (cons (car keys) new-table)
			       (cdr sub-table))))))
	  (if (eq? '() (cdr keys))
	      (set-cdr! sub-table
			(cons (cons (car keys) value)
			      (cdr sub-table)))
	      (let ((new-table (list '*table*)))
		(insert! (cdr keys) value new-table)
		(set-cdr! sub-table
			  (cons
			   (cons (car keys) new-table)
			   (cdr sub-table))))))
    'ok))
  (let ((local-table (list '*table*)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
	    ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys value local-table)))
	    ((eq? m 'debug) local-table)
	    (else (error "unknown operation: TABLE" m))))
    dispatch))
