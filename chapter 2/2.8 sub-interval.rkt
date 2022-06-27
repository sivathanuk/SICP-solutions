#lang sicp

(define (sub-interval x y)
  (let
      ((p1 (- (lower-bound x) (lower-bound y)))
       (p2 (- (lower-bound x) (upper-bound y)))
       (p3 (- (upper-bound x) (lower-bound y)))
       (p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (lower-bound a)
  (min (car a) (cdr a)))

(define (upper-bound a)
  (max (car a) (cdr a)))

(define (make-interval a b)
  (cons a b))

; check
(define a (cons 2 3))
(define b (cons 1 5))

(sub-interval a b)

; output
;(-3 . 2)
