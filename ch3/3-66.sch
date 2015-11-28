;; not mine

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers
  (integers-starting-from 1))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; 195 pairs preceed (1, 95). The (1, N) elemen is found at position ((N - 1) * 2) - 1 (or 2N - 3)
;; (99, 100) will after (3 * 2^(N -1 )) - 2 numbers if in the format (N, N -1) (too large to calculate)
;; (100, 100) (or (N,N) occurs ever 2^N - 2 pairs (too large to calculate)

;;0, 2, 6, 14, 
;;11 22, 33, 44
;; (2^N) - 2
