;; the scheme interpreter is showing that the last entry twice because the cdr of the queue is a reference to it
;; the deletion apparently fails to happen since the last pointer is only used if the actual queue (car) has any entries in it
;; which it does not

(define (print-queue queue)
  (if (empty-queue? queue)
      (display "empty queue")
      (display (car queue))))
