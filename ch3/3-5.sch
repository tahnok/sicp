(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range)))) 

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
           ((experiment)
            (iter (- trials-remaining 1)
                  (+ trials-passed 1)))
           (else
            (iter (- trials-remaining 1)
                  trials-passed))))
    (iter trials 0)) 

(define (area-of-rectangle x1 x2 y1 y2)
  (* (- (max x1 x2) (min x1 x2))
     (- (max y1 y2) (min y1 y2))))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (* (monte-carlo trials
                  (lambda
                      (predicate
                       (random-in-range x1 x2)
                       (random-in-range y1 y2))))
     (area-of-reactangle x1 x2 y1 2)))
