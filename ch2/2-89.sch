(define (empty-term-list) '())

(define (adjoin-term term term-list)
  (if (= (order term) (+ (order (first-term term-list)) 1))
      (cons term term-list) ;can't check zero since it's valid
      (adjoin-term term (adjoin-term (make-term (+ (order (first-term term-list)) 1) 0)))))

(define (first-term term-list)
  (make-term (length (- term-list 1)) (car term-list)))  
