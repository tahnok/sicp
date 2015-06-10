(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c))) ;warning will cause MIT-Scheme to loop forever!

;; Calculating (last-pair z) will also result in a loop since the last cdr of z points to z
