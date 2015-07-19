;; we will be storing this as a doubly linked list essentially
;; the deque will consist of a cons of the head and the tail
;; each entry in the deque will be a cons  of the item,
;; and another cons which contains a pointer to the "left" and "right"
;;
;; eg item => ('data, (#left-pointer, #right-pointer))
;;    deque => (#front-item, #rear-item)

(define (make-deque)
  (cons '() '())
  )

(define (left-pointer deque-item) (car (cdr deque-item)))

(define (right-pointer deque-item) (cdr (cdr deque-item)))

(define (data deque-item) car)

(define (head deque) car)
(define (tail deque) cdr)

(define (set-left! item new-left-item)
  (set-car! (cdr item) new-left-item))

(define (set-right! item new-right-item)
  (set-cdr! (cdr item) new-right-item))

(define (empty-deque? deque)
  (eq? (car deque) '()))


(define (front-deque deque)
  (if (empty-deque? deque)
      (error "empty deque!")
      (data (head deque))
  ))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "empty deque!")
      (data (tail deque))
  ))

(define (front-insert-deque! deque data)
  (if (empty-deque? deque)
      ((let (new-item (cons data (cons '() '()))))
       (cons new-item new-item))
      ((let (new-item (cons data (cons '() (front-deque deque)))))
       (begin
	 (set-left! (front-deque deque) new-item)
	 (cons new-item (rear-deque deque))))))


(define (rear-insert-deque! deque data)
  (if (empty-deque? deque)
      ((let (new-item (cons data (cons '() '()))))
       (cons new-item new-item))
      ((let (new-item (cons data (cons (rear-deque deque) '()))))
       (begin
	 (set-right! (rear-deque deque) new-item)
	 (cons (front-deque deque) new-item)))))

(define (front-delete-deque! deque)
  (if (empty-deque? (right-pointer (front-deque deque)))
      (make-deque)
      ((let (new-front (right-pointer (front-deque deque))))
       (begin
	 (set-left! new-front '())
	 (cons new-front (rear-deque deque))))))

(define (rear-delete-deque! deque)
  (if (empty-deque (left-pointer (rear-deque deque)))
      (make-deque)
      (let ((new-rear (left-pointer (rear-deque deque))))
	(begin
	  (set-right! new-rear '())
	  (cons (front-deque deque) new-rear)))))
	
