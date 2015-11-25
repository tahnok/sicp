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
;; (99, 100) will be after ... the pattern is (N, N+1) is at (3 * N) + last one (3 * 2^N) - 1
;; see 1, 4, 10, 22, 46 or maybe 2, 5, 11, 23, 47
