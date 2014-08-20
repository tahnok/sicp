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

(define (weigh-branch branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
	structure
	(total-weight structure))))

(define (total-weight mobile)
  (+ (weigh-branch (left-branch mobile)) (weigh-branch (right-branch mobile))))


(define (balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and
     (=
      (* (branch-length left) (weigh-branch left))
      (* (branch-length right) (weigh-branch right)))
     (balanced? left)
     (balanced? right))))



;; test cases

  
 (define level-1-mobile (make-mobile (make-branch 2 1) 
                                     (make-branch 1 2))) 
 (define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                     (make-branch 9 1))) 
 (define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                     (make-branch 8 2))) 
