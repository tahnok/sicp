;a

(segments->painter (list
		    (make-segment (make-vect 0 0) (make-vect 0 1))
		    (make-segment (make-vect 0 0) (make-vect 1 0))
		    (make-segment (make-vect 0 1) (make-vect 1 1))
		    (make-segment (make-vect 1 0) (make-vect 1 1))))

;b

(segments->painter (list
		    (make-segment (make-vect 0 0) (make-vect 1 1))
		    (make-segment (make-vect 1 0) (make-vect 0 1))))

;c

(segments->painter (list
		    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
		    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
		    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
		    (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

;d


(segments->painter (list
		    (make-segment (make-vect 0 0.7) (make-vect 0.2 0.5))
		    (make-segment (make-vect 0.2 0.5) (make-vect 0.35 0.6))
		    (make-segment (make-vect 0.35 0.6) (make-vect 0.3 0)) ;bottom left
		    (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
		    (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0)) ; bottom middle
		    (make-segment (make-vect 0.7 0) (make-vect 0.65 0.6))
		    (make-segment (make-vect 0.65 0.6) (make-vect 1 0.3)) ; bottomw right
		    (make-segment (make-vect 1 0.4) (make-vect 0.7 0.8))

