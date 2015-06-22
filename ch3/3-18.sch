(define (contains-loop? suspect)
  (let ((seen ()))
    (define (inner suspect)
      (cond
       ((not (pair? suspect)) false)
       ((memq seen (cdr suspect)) true)
       (else
	(begin
	  (set! seen (cons seen suspect))
	  (contains-loop? (cdr suspect))
	  ))))
    (inner suspect)))
