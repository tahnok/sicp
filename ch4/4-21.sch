;; not mine

((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)

;; a)

((lambda (n)
 ((lambda (fib) (fib fib n))
  (lambda (fb k) (if (or (= k 1) (= k 0)) 1 (+ (fb fb (- k 1)) (fb fb (- k 2)))))))
 10)


;; this works because the first lambda enables you to pass in a reference to the second lambda that refers to itself. Normally in the body of a lambda there is no access to a binding that references that lambda. We skirt around that by having the inner lambda accept an argument to itself basically

;; b)

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
