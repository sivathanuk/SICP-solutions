#lang sicp

(define tolerance 0.00001)

(define (fixed-point f guess)
  (define (close-enough x y)
    (< (abs (- x y)) tolerance))
  (define (try-it g)
    (let
        ((next (f g)))
      (if (close-enough next g)
          g
          (try-it next))))
  (try-it guess))

(fixed-point cos 1)
; 0.7390893414033928

; Golden ratio

(fixed-point (lambda (x)
               (+ 1.0
                  (/ 1 x)))
             1)

; 1.6180371352785146
