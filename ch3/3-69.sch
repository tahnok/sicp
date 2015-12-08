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
;; mine
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s)
	 (stream-car t)
	 (stream-car u))
    (interleave
     (stream-map (lambda (x) (append (list (stream-car s)) x))
		 (stream-cdr (pairs t u)))
     (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define (pythagorean-triple i j k)
  (= (+ (* i  i) (* j j)) (* k k)))
  
  

(define pythagorean-triples
  (stream-filter
   (lambda (item) (apply pythagorean-triple item))
   (triples integers integers integers)))
