(define (serialized-exchange account1 account2)
  (let ((first (if (> (account1 'id) (account2 'id)) account1 account2))
	(second (if (< (account1 'id) (account2 'id)) account1 account2)))
    (let ((serializer1 (first 'serializer))
	  (serializer2 (second 'serializer)))
      ((serializer1 (serializer 2 exchange))
       first
       second))))

;; add a (let ((id (get-unique-account-id))) ...) around all of (make-account-and-serializer) and define
;; (get-unique-account-id) to hand out sequential ids using a mutex
      
