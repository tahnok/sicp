(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (for-each
	  (let ((type1 (car type-tags)))
	    (if
	     (every (lambda (type) (get-coercion type1 type)))
