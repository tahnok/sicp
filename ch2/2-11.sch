;there are 9 cases, but only if you don't think of multiplication as commutitive
;  1 2     3 4
; (+,+) x (+,+) 1 x 3, 2 x 4
; (-,-) x (-,-) 1 x 3, 2 x 4

; (+,+) x (-,-) 2 x 4, 1 x 3
; (-,-) x (+,+) 2 x 3, 1 x 3

; (+,+) x (-,+) 2 x 3, 2 x 4
; (-,-) x (-,+) 1 x 4, 1 x 3
; (-,+) x (+,+) 1 x 4, 2 x 4
; (-,+) x (-,-) 2 x 4, 1 x 4

; (-,+) x (-,+) <--- special case, 1x3 may be greater than 2x4 and 1x4 may be smaller than 2x3



(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (same-sign x)
  (or (and (> (lower-bound x) 0) (> (upper-bound x) 0)) (and (< (lower-bound x) 0) (< (upper-bound x) 0))))

(define (mul-interval x y)
  (let ((lower-x (lower-bound x))
	(upper-x (upper-bound x))
	(lower-y (lower-bound y))
	(upper-y (upper-bound y)))
  (cond
   ((and (same-sign x) (same-sign y))
    (make-interval (* lower-y lower-y) (* upper-x upper-y)))
   ((and (> lower-x 0) (> upper-x 0))
    (make-interval (* upper-x lower-y) (* upper-x upper-y)))
   ((and (< lower-x 0) (< lower-x 0))
    (make-interval (* lower-x upper-y) (* lower-x lower-y)))
   ((and (> lower-y 0) (> upper-y 0))
    (make-interval (* lower-x upper-y) (* upper-x upper-y)))
   ((and (< lower-y 0) (< upper-y 0))
    (make-interval (* upper-x upper-y) (* lower-x upper-y)))
   (else (make-interval (min (* upper-x upper-y) (* upper-x lower-y)) (max (* lower-x lower-y) (* upper-x upper-y)))))))
