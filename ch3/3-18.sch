(define (contains-loop? suspect)
  (let ((seen ()))
    (define (inner suspect)
      (cond
       ((and (pair? suspect) (memq seen (cdr suspect)))
	true)
       ((not (pair? suspect)) false)
       (else
	(begin
	  (set! seen (cons seen suspect))
	  (contains-loop? (cdr suspect))))))
    (inner suspect)))
