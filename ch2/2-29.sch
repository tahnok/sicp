 ;; Given: 
 (define (make-mobile left right) 
   (list left right))
 (define (make-branch length structure) 
   (list length structure)) 

;a

(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

;b

(define (total-weight mobile)
  (+
   (if (not (pair? (branch-structure (left-branch mobile))))
       (branch-structure (left-branch mobile))
       (total-weight (branch-structure (left-branch mobile))))
   (if (not (pair? (branch-structure (right-branch mobile))))
       (branch-structure (right-branch mobile))
       (total-weight (branch-structure (right-branch mobile))))))
