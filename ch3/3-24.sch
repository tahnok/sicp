(define (make-table same-key?)
  (define (contains key values)
    (if (eq? values '())
	false
	(if (same-key? (caar values) key)
	    (car values)
	    (contains key (cdr values)))))
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record
	     (contains key (cdr local-table))))
	(if record (cdr record) false)))
    (define (insert! key value)
      (let ((record
	     (contains key (cdr local-table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! local-table
		      (cons (cons key value)
			    (cdr local-table)))))
      'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
	  ((eq? m 'insert-proc!) insert!)
	  (else (error "unknown operation: TABLE" m))))
  dispatch))
