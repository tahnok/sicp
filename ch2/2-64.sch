					;from book

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (n = 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result
	       (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result
		   (partial-tree
		    (cdr non-left-elts)
		    right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts
		     (cdr right-result)))
		(cons (make-tree this-entry
				 left-tree
				 right-tree)
		      remaining-elts))))))))

					;partial tree works by first taking the (n-1)/2 elements of the list and recursively turning it into a tree, then taking the first element in the rest of the elements as the node for this tree, then turning the remaining elements into a tree.
					;the code will return an empty list when n is zero, and will return a leaf node when n is one since it has two empty lists for the left and right trees.

					; importantly, this will properly return an unchanged list of elements if the passed in list has less than n elements

;order of growth:
