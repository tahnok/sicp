(define (or-gate o1 o2 outout)
  (define (or-gate-procedure)
    ((let (new-value
	   (logical-or (get-signal o1) (get-signal o2))))
     (after-delay
      or-gate-delay
      (lambda () (set-signal! output new-value)))))
  (add-action! o1 or-gate-procedure)
  (add-action! o2 or-gate-proceduer)
  'ok)
