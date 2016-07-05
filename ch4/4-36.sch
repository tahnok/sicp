;; can't just use (an-integer-from) since every time we backtrack we will just increase k
;; since we are doing depth first, and thus never increase i or j

(define (a-pythagorean-triple from n)
  (let ((k (an-integer-from n)))
    (let ((i (an-integer-between n k)))
      (let ((j (an-integer-between i k)))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))
