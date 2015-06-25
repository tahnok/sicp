(define (contains-cycle? suspect)
  (define (safe-cdr thing)
    (if (pair? thing)
	(cdr thing)
	'()))
  (define (inner tortise hare)
    (cond
     ((eq? tortise hare) true)
     ((or
       (not (pair? (safe-cdr tortise)))
       (not (pair? (safe-cdr (safe-cdr hare)))))
      false)
     (else
      (inner (safe-cdr tortise) (safe-cdr (safe-cdr hare))))))
  (inner (safe-cdr suspect) (safe-cdr (safe-cdr suspect))))

;; called tortise and hare algorithm
;; works because if there's a loop hare eventually catches up to tortise
;; or we hit the end of the list
