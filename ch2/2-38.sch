(fold-right / 1 (list 1 2 3))
(/ (/ (/ 3 1) 2) 1)
;=> 3/2

(fold-left / 1 (list 1 2 3))
(/ (/ (/ 1 1) 2) 3)

; 1/6

(fold-right list nil (list 1 2 3))

;(1 (2 (3 ())))

(fold-left list nil (list 1 2 3))

;(3 (2 (() 1)))

;op needs to commutative (ie: (op a b) == (op b a))
