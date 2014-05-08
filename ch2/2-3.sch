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

;2.3

(define (make-rectangle top-right bottom-left)
  (cons top-right bottom-left))

(define (top-right rectangle)
  (car rectangle))

(define (bottom-left rectangle)
  (cdr rectangle))

(define (top-left rectangle)
  (make-point (x-point (top-right rectangle)) (y-point (bottom-left rectangle))))

(define (bottom-right rectangle)
  (make-point (x-point (bottom-left rectangle)) (y-point (top-right rectangle))))

(define (verticle-length rectangle)
  (abs (- (y-point (top-right rectangle)) (y-point (bottom-right rectangle)))))

(define (horizontal-length rectangle)
  (abs (- (x-point (top-right rectangle)) (x-point (top-left rectangle)))))

(define (perimeter rectangle)
  (* 2
     (+
      (horizontal-length rectangle)
      (verticle-length rectangle))))


(define (area rectangle)
  (* (verticle-length rectangle) (horizontal-length rectangle)))
  
