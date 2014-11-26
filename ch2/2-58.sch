;a

(define (sum? exp) (and (pair? exp) (eq? '+ (cadr exp))))

;just need to check 2nd item for operator and 1 and 3rd for addend and augend or multiplier and multiplicand

;b

;need to be able to determine that the (x + 3 * (x + y + 2)) is first a product of (x + 3) and (x + y + 2) and that (x + y + 2) is a sum (for simplicty's sake) of x and (y + 2)

;so an expression is a product if we look through the elements of the expression/list and see if any are a *. We split the expression into multiplier and multiplicand on the first occurance of a *. If no * is found then it's not a product. 

;the expression is a sum if it's not a produt AND it contains a +. If it contains a * then it must not be evaluated is a sum yet.


(define (product? expression) (and (pair? expression) (any? (lambda (x) (eq? x '*)) expression)))

(define (sum? expression) (and (pair? expression) (not (product? expression)) (any? (lambda (x) (eq? x '+)))))

(define (multiplier expression)
  (memq something))
