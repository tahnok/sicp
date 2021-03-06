(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map sub-tree proc)
	     (proc sub-tree)))
       tree))
