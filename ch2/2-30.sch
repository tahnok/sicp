;higher order

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

;direct

(define (square-tree2 tree)
  (if (pair? tree)
      (cons (square-tree2 (car tree)) (square-tree (cdr tree)))
      (* tree tree)))
