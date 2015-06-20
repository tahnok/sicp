(define (contains-loop? suspect)
  (let ((seen ()))
    (define (inner suspect)
      (cond
       ((and (pair? suspect) (memq seen (cdr suspect)))
	false)
       (else
	  (
