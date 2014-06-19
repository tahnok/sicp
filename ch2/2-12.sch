(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;-- my work

(define (make-center-percent center percent)
  (let ((width (* center (/ percent 100))))
	(make-interval (- center width) (+ center width))))

(define (percent interval)
  (* (- (/ (upper-bound interval) (center interval)) 1) 100))
