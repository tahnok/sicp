(define (make-deque)
  (cons '() '())
  )

(define (left item) (car (cdr item)))

(define (right item) (cdr (cdr item)))

(define (set-left! entry left-item)
  (set-car! (cdr item) left-item))

(define (set-right! entry right-item)
  (set-cdr! (cdr item) right-item))

(define (empty-deque? deque)
  (eq? (car deque) '()))


(define (front-deque deque)
  (if (empty-deque? deque)
      (error "empty deque!")
      (car (car deque))
  ))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "empty deque!")
      (car (cdr deque))
  ))

(define (front-insert-deque! deque item)
  (if (empty-deque? deque)
  )

(define (rear-insert-deque! deque item)
)
