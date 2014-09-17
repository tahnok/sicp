;old

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

;new

(define (uniq-triples n)
  (flatmap
   (lambda (x)
     (map
      (lambda (pair)
	(list (car pair) (cadr pair) x)
	)
      (uniq-pair (- x 1))))
   (enumerate-interval 3 n)))

(define (matching-triple triple n)
  (= (accumulate + 0 triple) n))

(define (uniq-matching-triples n s)
  (filter
   (lambda (triple)
     (matching-triple triple s))
   (uniq-triples n)))

; triples for n = 3
;(1 2 3)

; triples for n = 4
;(1 2 3) (1 2 4) (1 3 4) (2 3 4)

; triples for n = 5
;(1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 5) (2 3 5) (1 3 5) (1 4 5) (3 4 5) (2 4 5)
