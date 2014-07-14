(define (fringe lst)
  (define (fringe-iter lst acc)
    (cond ((null? lst) acc)
	  ((not (pair? lst)) (append acc (list lst)))
	  (else
	   (fringe-iter (cdr lst) (fringe-iter (car lst) acc)))))
  (fringe-iter lst (list)))

(define (fringe2 lst)
    (cond ((null? lst) (list))
	  ((not (pair? lst)) (list lst))
	  (else
	   (append (fringe (car lst)) (fringe (cdr lst))))))
