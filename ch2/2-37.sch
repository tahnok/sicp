;supplied

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accmumlate op init (map car seqs))
	    (accumulate-n op init (map cd seqs)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
	  (accumulate op init (cdr seq)))))


(define matrix
  (list (list 1 2 3 4)
	(list 4 5 6 6)
	(list 6 7 8 9)))
;ours

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
