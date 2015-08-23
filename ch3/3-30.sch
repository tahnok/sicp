(define (ripple-carry-adder a-list b-list s-list carry)
  (define (ripple a-wires b-wires s-wires previous-carry)
    (if (null? (cdr a-wires))
	(full-adder (car a-wires) (car b-wires) previous-carry (car s-wires) carry)
	(let ((new-carry (make-wire)))
	  (full-adder (car a-wires) (car b-wires) previous-carry (car s-wires) new-carry)
	  (ripple (cdr a-wires) (cdr b-wires) (cdr s-wires) new-carry))))
  (ripple a-list b-list s-list (new-wire)))

;; delay is n * full-adder delay
