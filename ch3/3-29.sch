(define (or-gate2 o1 o2 output)
  (let (
	(no1 (make-wire))
	(no2 (make-wire))
	(andout (make-wire)))
    (let (
	  (not1 ((inverter o1 no1)))
	  (not2 ((inverter o2 no2)))
	  (and1 ((and-gate not1 not2 andout)))
	  (not3 ((inverter andout output))))
  (define (or-gate-procedure)
    ((let
	 (new-value (get-signal
