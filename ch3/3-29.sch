(define (or-gate2 o1 o2 output)
  (let (
	(no1 (make-wire))
	(no2 (make-wire))
	(andout (make-wire)))
    (inverter o1 no1)
    (inverter o2 no2)
    (and-gate not1 not2 andout)
    (inverter andout output)
    'ok))

;;; delay is and-gate-delay + 2 inverter-gate-delay because the first 2 inverters happen at the same time

