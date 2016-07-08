(define (multiple-dwelling)
  (let ((fletcher (amb 2 3 4)))
    (let ((cooper (amb-not-contains (list 2 3 4 5) (list fletcher))))
      (let ((baker (amb-not-contains (list 1 2 3 4) (list fletcher cooper))))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(let ((miller (an-integer-between (+ 1 cooper) 5)))
	  (require (memq miller (list fletcher cooper baker)))
	  (let ((smith (amb
			(an-integer-between 1 (- fletcher 2))
			(an-integer-between (+ fletcher 2) 5))))
	    (require smith (list fletcher cooper baker miller))
	    (list
	     (list 'baker baker)
	     (list 'cooper cooper)
	     (list 'fletcher fletcher)
	     (list 'miller miller)
	                      (list 'smith smith))))))))
		      
	
      
(define (amb-not-contains list excluded)
  (filtered-amb (lambda (x) (memq x excluded))))

(define (filtered-amb filter-proc list)
  (apply amb (filter filter-proc list)))
