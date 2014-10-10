;
(define (lst-origin frame)
  (car frame))

(define (lst-edge1 frame)
  (cadr frame))

(define (lst-edge2 frame)
  (caddr frame))

(define (cons-origin frame)
  (car frame))

(define (cons-edge1 frame)
  (cadr frame))

(define (cons-edge2 frame)
  (cddr frame))
