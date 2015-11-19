(define (stream-limit stream tolerance)
  (if (< (abe (- (stream-car stream) (stream-car (stream-cdr stream)))) tolerance)
      (stream-car (stream-cdr stream))
      (stream-limit (stream-cdr stream) tolerance)))
