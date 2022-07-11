#lang sicp


; last pair
(define (last items)
  (if (null? (cdr items))
      (car items)
      (last (cdr items))))

(last (list 1 8 27))