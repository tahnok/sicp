(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

					; actual code

(define (encode-symbol symbol tree)
  (cond
   ((equal? (symbols tree) (list symbol)) '())
   ((member symbol (symbols (left-branch tree)))
    (append '(0) (encode-symbol symbol (left-branch tree))))
   (else
    (append '(1) (encode-symbol symbol (right-branch tree))))))
