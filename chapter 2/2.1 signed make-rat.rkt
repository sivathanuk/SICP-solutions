#lang sicp
; helper functions
(define (gcd n d)
  (if (= (remainder n d) 0)
      (abs d)
      (gcd d (remainder n d))))

(define (sign a)
    (if (< a 0)
        -1
        1))

;(gcd 21 35)

(define (Make-rat x y)
  (let
      ((g (gcd x y)))
    (cons (* (sign x) (sign y) (abs (/ x g)))
          (abs (/ y g)))
    ))

(define (print-rat z)
  (newline)
  (display (car z))
  (display "/")
  (display (cdr z)))


(print-rat (Make-rat -3 4))
(print-rat (Make-rat 3 -4))
(print-rat (Make-rat -3 -4))
(print-rat (Make-rat 3 4))
(newline)
(print-rat (Make-rat -40 4))
(print-rat (Make-rat 1250 -25))
(print-rat (Make-rat 5 25))



#| output:

-3/4
-3/4
3/4
3/4
-10/1
-50/1
1/5

|#
