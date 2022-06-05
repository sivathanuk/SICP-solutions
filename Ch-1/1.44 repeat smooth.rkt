#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

(define dx 0.0001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define square (lambda (x)
                 (* x x)))

(define (n-fold-smooth f n)
  (lambda (x)
    ((repeated (smooth f) n) x)))

; check

((repeated (smooth square) 2) 3)

((n-fold-smooth square 2) 3)
        


