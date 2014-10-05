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

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map
	     (lambda (new-row) (adjoin-position new-row k rest-of-queens))
	     (enumerate-interval 1 board-size))
	    (queen-cols (- k 1)))))))
  (queen-cols board-size))

;mine

;use lists

(define (empty-board) nil)

(define (safe? k positions)
  (and
   (row-safe? k posititions)
   (up-diagonal-safe? k positions)
   (down-diagonal-safe? k posititions)
   ))

(define (row-safe? k positions)
  (let ((head (car positions))
	(tail (cdr positions)))
    (not (accumulate
     (lambda (x y)
       (if y
	   true
	   (= head x)))
     false
     tail))))

(define (up-diagonal-safe? k positions)
  (define (check positions k)
    (cond
     ((= k 0) true)
     ((= (car positions) head) false)
     (else
      (check (cdr positions (- k 1))))))
  (let ((head (car posistions)))
    (check (cdr positions) head)))



(define (adjoin-position new-row k rest-of-queens)
p  (append (list new-row) rest-of-queens)
  )
