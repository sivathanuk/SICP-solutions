#lang sicp

(define x (list 1 2 3))
(define y (list 4 5 6))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(append x y)
; (1 2 3 4 5 6)

(cons x y)
;((1 2 3) (4 5 6)) - wrong
;((1 2 3) 4 5 6)

(list x y)
; ((1 2 3) (4 5 6))

