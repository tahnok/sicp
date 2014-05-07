(define (make-segment start end)
  (cons start end))

(define (start segment)
  (car segment))

(define (end segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint segment)
  (make-point
   (average (x-point (start segment)) (x-point (end segment)))
   (average (y-point (start segment)) (y-point (end segment)))))

(print-point
 (midpoint (make-segment (make-point 2 3) (make-point 4 5))))


;utils
(define (average a b)
  (/ (+ a b) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

