(define (lookup given-key tree-of-records)
  (if
   ((empty? tree-of-records)
    false
    (let ((current-key (key (entry tree-of-records))))
      (cond
       ((equal? current-key given-key)
	(entry tree-of-records))
       ((> current-key given-key)
	(lookup given-key (right-branch tree-of-records)))
       (else
	(lookup given-key (left-branch tree-of-records))))))))
