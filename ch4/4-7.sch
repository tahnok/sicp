;;

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-args exp) (cdr exp))
(define (definitions* args) (car args))
(define (first-var definitions) (caar definitions))
(define (first-body definitions) (cadar definitions))
(define (rest-let* definitions) (cdr definitions))
(define (body* args) (cdr args))
(define (last-let*? definitions) (null? (cdr definitions)))

(define (let*->nested-lets exp)
  (expand-let* (let*-args exp)))

(define (expand-let* args)
  (if (last-let*? (definitions* args))
      (let->combination args)
      (expand-let-clauses
       (list (car (definitions* args))
	     (list 'let*
		   (cdr (definitions* args))
		   (body* args))))))
