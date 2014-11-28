(define (element-of-set? x set)
  (cond
   ((null? set) false)
   ((equal? x (car set)) true)
   (else
    (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (append set (list x)))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond
   ((null? set1) set2)
   ((element-of-set? (car set1) set2)
    (intersection-set (cdr set1) (adjoin-set (car set1) set2))
   (else
    (intersection-set (cdr set1) set2)))))


;well union and adjoin set are much faster if you allow duplicates, but element-of-set may tend to take longer since sets are not stored as efficiently and you will have multiple checks against the same element while traversing the set
