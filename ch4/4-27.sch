;; imagine a delayed scheme here

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

count
;;; 0
;;; nothing w has not be forced since it has not been referenced, thus count is unchanged

w
;;; 10
;;; w returns 10 since id return it's argument (even when called recursively)

count
;;; 2
;;; now set to 2 since id was called twice in order to resolve w
;;; strange behaviour since count changes after w is referenced,
;;; not when defined or even first used (since w will be memoized, count won't be changed again)
