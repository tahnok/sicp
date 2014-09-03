(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accmumlate op init (map car seqs))
	    (accumulate-n op init (map cd seqs)))))
