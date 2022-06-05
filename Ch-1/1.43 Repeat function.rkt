#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        f
        (repeated (compose f f) (- n 1))))


(define (square n)
  (* n n))

; check
((repeated square 2) 5)
((repeated cos 25) 0.739)
((repeated inc 5) 5)

#|Output:

625
0.7390851332151605
21

|#
