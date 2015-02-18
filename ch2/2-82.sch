(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (let ((coerce-args (coerce-all args '())))
	    (if coerced-args
		(apply-generic op coerced-args)
		(error "no coercion found for types" type-tags)))))))

(define (coerce-args args tried)
  (if
   (any? args)
   (let ((target (car args))
	 (rest (append (cdr args) tried)))
     (let ((coerced-maybe
	    (map (lambda (arg)
		   (let ((t1->t2 (get-coercion (type-tag target) (type-tag arg))))
		       (if t1->t2
			   (t1->t2 arg)
			   #f))
		 (cdr args)))))
       (if (memq #f coerced-maybe)
	   (coerce-args op rest (append rest tried))
	   (append tried args))))
   #f))
