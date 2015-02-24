(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (card args)))
		(if (not (= type1 type2))
					;code for raising here
		    (cond
		     ((can-be-raised type1 type2) (apply-generic op (apply-generic 'raise a1) a2))
		     ((can-be-raised type2 type1) (apply-generic op a1 (apply-generic 'raise a2)))
		     (else
		      (error "No common type for types" type-tags)))
		    (error "Can't coerce types if they are the same")))
	      (error "No method for these types"
		     (list op type-tags)))))))

(define (can-be-raised lower higher)
  ????????????)
