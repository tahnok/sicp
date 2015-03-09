(define (project x) (apply-generic 'project x))

(define (install-scheme-number)
  (put 'project '(scheme-number)
       (lambda (x)
	 x)))

(define (install-rational-number)
  (put 'project '(rational)
       (lambda (x)
	 (make-int (round (/ (numer x) (denom x)))))))

(define (install-real)
  (put 'project '(real)
       (lambda (x)
	 (apply make-rat (rationalize x))))) ;rationalize is an imaginary function that takes a real number and attempts to reduce it to a real. It's not clear how we would store a real like pi though, so...

(define (install-complex-number)
  (put 'project '(complex)
       (lambda (x)
	 (make-real (real-part x)))))


(define (drop x)
  (if
   (equ? (project (raise x)) x)
   (drop (project x))
   x))

;now we can simple drop one arg, see if it's possible to raise the other args to it. If not we iterate again and see how low the next arg can be dropped &tc

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
		     ((can-be-raised type1 a1 (type-tag (drop a2)) (apply-generic op (apply-generic 'raise a1) a2))
		     ((can-be-raised type2 a2 (type-tag (drop a1)) (apply-generic op a1 (apply-generic 'raise a2)))
		     (else
		      (error "No common type for types" type-tags)))
		    (error "Can't coerce types if they are the same")))
	      (error "No method for these types"
		     (list op type-tags)))))))

(define (can-be-raised lower-type element higher)
  (let ((proc (get 'raise lower)))
    (if proc
	(let ((raised (proc lower element)))
	  (if (= higher (type-tag raised))
	    #t
	    (can-be-raised (type-tag raised) raised higher)))
	#f)))
