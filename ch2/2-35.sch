(define (count-leave t)
  (accumulate
   +
   0
   (map
    (lambda (node)
      (if (pair? node)
	  (count-leaves node)
	  1))
    t)))
