(define (text-of-quotation exp)
  (let ((text cadr exp))
    (if (list? (cadr text))
	(eval (make-fancy-list text))
	text)))

(define (make-fancy-list list)
  (if (null? list)
      ''()
      (list 'cons
	    (list 'quote (car list))
	    (make-fancy-list (cdr list)))))
;; older version   (define (text-of-quotation exp) (cadr exp))
