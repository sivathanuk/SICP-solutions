#lang sicp

; repeated - recursion

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))
