(define song (generate-huffman-tree
 '(
   (a 2)
   (boom 1)
   (get 2)
   (job 2)
   (sha 3)
   (na 16)
   (wah 1)
   (yip 9)
   )))

(define result (encode '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom) song))

(length result) ;84

;fixed length code: 8 combos (3 bits to encode 8 messages * 36 words = 108)
