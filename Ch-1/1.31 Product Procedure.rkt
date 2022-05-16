#lang sicp

; prod

(define (product term a next b)
  (if (> a b) 1
      (* (term a) (product term (next a) next b))))
      
; product procedure iteration

(define (product-i term a next b)
  (define (prod-iter a result)
    (if (> a b)
        result
        (prod-iter (next a) (* result (term a)))))
  (prod-iter a 1))
