;; a.)

(define (integrate-series series)
  (stream-map * (stream-map / ones integers) series))

;; b.)

(define cosine-series
  (cons-stream 1 (scale-stream (integrate sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate cosine-series)))
