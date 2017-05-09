(define adjectives '(nice hot cold bad red blue hoppy))

(define (adjective-noun)
  (define (maybe-extend adjective-noun)
    (amb adjective-noun
	 (maybe-extend (list 'adjective-noun
			     (parse-adjective)
			     adjective-noun))))
  (maybe-extend (parse-noun-phrase)))

;;; from scheme wiki

(define (parse-simple-noun-phrase)       
  (amb (list 'simple-noun-phrase 
	     (parse-word articles) 
	     (parse-word nouns)) 
       (list 'simple-noun-phrase 
	     (parse-word articles) 
	     (parse-word adjectives) 
	     (parse-word nouns)))) 
