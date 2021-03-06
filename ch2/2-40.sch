;given

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
	  (accumulate op init (cdr seq)))))
;ours

(define (uniq-pair n)
  (flatmap
   (lambda (x)
     (map
      (lambda (y)
	(list y x))
      (enumerate-interval 1 (- x 1))
      )
     )
   (enumerate-interval 1 n)
   )
  )


;new prime-sum-pairs

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (uniq-pair n))))
