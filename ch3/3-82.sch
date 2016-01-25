;; not mine

(define (montre-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (area-of-rectangle x1 x2 y1 y2)
  (* (- (max x1 x2) (min x1 x2))
     (- (max y1 y2) (min y1 y2))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range)))) 

;; mine

(define (random-in-range-stream min max)
  (cons-stream
   (random-in-range min max)
   (random-in-range-stream min max)))

(define (pairs a b)
  (list (stream-car a) (stream-car b))
  (pairs (stream-cdr a) (stream-cdr b)))

(define (estimate-integral predicate x1 x2 y1 y2)
  (let ((area (area-of-rectangle x1 x2 y1 y2)))
    (define random-valid-pairs
      (pairs
       (random-in-range-stream x1 x2)
       (random-in-range-stream y1 y2)))
    (stream-map (lambda (p)
		  (* p area))
		(monte-carlo (stream-map predicate random-valid-pairs) 0 0))))
