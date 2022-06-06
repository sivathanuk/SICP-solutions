#lang sicp

; repeat function - recursion

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))
      

; repeat fucntion - iteration

(define (repeat-i f n)
  (define (repeat-iter result k)
    (if (= k 1)
        result
        (repeat-iter (compose result f) (- k 1))))
  (repeat-iter f n))
