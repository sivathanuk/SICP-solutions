#lang sicp

(define tolerance 0.0001)

(define (fixed-point f guess)
  (define (close-enough x y)
    (< (abs (- x y)) tolerance))
  (define (try-it g)
    (let
        ((next (f g)))
      (display g)
      (newline)
      (if (close-enough next g)
          g
          (try-it next))))
  (try-it guess))

; without damping
(fixed-point (lambda (x)
               (/ (log 1000)
                  (log x)))
             4)

; with damping
(fixed-point (lambda (x)
               (/ (+ (/ (log 1000)
                        (log x))
                     x)
                  2))
             4)
