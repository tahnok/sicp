(define (make-accumulator count)
  (lambda (new)
    (begin
      (set! count (+ count new))
      count)))
    
