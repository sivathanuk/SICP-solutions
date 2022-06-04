#lang sicp


(define (square n)
  (* n n))

(define (cube n)
  (* n (square n)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)
; 49

((compose cube square) 2)
; 64
