(define (install-polynomial-package)
  (define (empty-sparse-list)
    '(sparse ()))
  (define (first-term-sparse term-list)
    (car term-list))
  (define (rest-term-sparse term-list)
    (cdr term-list))
  (define (adjoin-term-sparse term termlist)
    (if (=zero? (ceoff term))
	term-list
	(cons term term-list)))

  (define (empty-dense-list)
    '(dense ()))
  (define (first-term-term term-list)
    (make-term (length (- term-list 1)) (car term-list)))
  (define (rest-term-dense term-list)
    (cdr term-list))
  (define (adjoin-term-sparse term term-list)
  (if (= (order term) (+ (order (first-term term-list)) 1))
      (cons term term-list) ;can't check zero since it's valid
      (adjoin-term term (adjoin-term (make-term (+ (order (first-term term-list)) 1) 0)))))

  (define (empty-termlist? termlist)
    (= (cadr termlist) '()))
  (define (first-term termlist)
    (cond
     ((= 'sparse (type termlist))
      (first-term-sparse (cadr termlist)))
     ((= 'dense (type termlist))
      (first-term-dense (cadr termlist)))
     (else
      (error "unknown type"))))

  (define (rest-term termlist)
    (cond
     ((= 'sparse (type termlist))
      (rest-term-sparse (cadr termlist)))
     ((= 'dense (type termlist))
      (rest-term-dense (cadr termlist)))
     (else
      (error "unknown type"))))
  (define (rest-terms termlist) (cdr term-list))
  
  (define (adjoin-term term termlist)
    (cond
     ((= 'sparse (type termlist))
      (adjoin-term-sparse term (cadr termlist)))
     ((= 'dense (type termlist))
      (rest-term-dense term (cadr termlist)))
     (else
      (error "unknown type"))))
  
  (define (the-empty-termlist))
  )
