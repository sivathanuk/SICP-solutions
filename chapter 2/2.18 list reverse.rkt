#lang sicp

; reverse
(define (reverse items)
  (define (rev-sub items l)
    (if (null? items)
        l
        (rev-sub (cdr items) (cons (car items) l))))
  (rev-sub items (list )))
