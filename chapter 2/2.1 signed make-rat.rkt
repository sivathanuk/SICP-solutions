
#lang sicp

(define (gcd n d)
  (if (= (remainder n d) 0)
      (abs d)
      (gcd d (remainder n d))))

;(gcd 21 35)

(define (Make-rat x y)
  (let
      ((g (gcd x y)))
    (if (or
         (and (negative? x) (positive? y))
         (and (positive? x) (negative? y)))
        (cons (- (abs x)) (abs y))
        (cons (abs x) (abs y)))))

(define (print-rat z)
  (newline)
  (display (car z))
  (display "/")
  (display (cdr z)))

(print-rat (Make-rat -3 4))
(print-rat (Make-rat 3 -4))
(print-rat (Make-rat -3 -4))
(print-rat (Make-rat 3 4))


#| output:
-3/4
-3/4
3/4
3/4 |#
