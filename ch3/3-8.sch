;;;make (+ (f 0) (f 1)) = 0 if left to right, and = 1 if right to left

(define z -1)

(define (f input)
  (if (= z -1)
      (set!
