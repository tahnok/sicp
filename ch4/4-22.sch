(define (analyze-let exp)
  (let ((lvars (vars exp))
	(lvals (map analyze (bodies exp)))
	(lbody (analyze (body exp))))
    (lambda (env)
      (execute-application
       (make-lambda lvars lbody)
       (extend-environment lvals exp)))))
;; or simpler....

(define (analyze-let exp)
  (analyze (let->combination exp)))
