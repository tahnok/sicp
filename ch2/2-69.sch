(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (merge-tree (make-code-tree (make-leaf (car leaf-set)) (make-leaf (cadr leaf-set))) (caddr leaf-set)))

(define (merge-tree tree leaf-set)
  (if
   (empty? leaf-set)
   tree
   (mergre-tree (make-code-tree tree (make-
