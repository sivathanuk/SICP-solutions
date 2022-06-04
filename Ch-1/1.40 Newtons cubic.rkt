#lang sicp

(define (square x)
  (* n n))

(define (cube x)
  (* x x x))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

; newton's transformation
(define (derive f)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx)))

(define (newtons-method g)
  (lambda (x)
    (- x
       (/ (g x)
          ((derive g) x)))))



