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
  
  ; pi/4 convergence
  
  ; recursive
  (define (prod-proc a b)
  (define (term-p n)
  (if (= n 1)
      (/ 2 3)
      (/ 1 (- 1
              (/ 1.0 (* 4 (sq n)))))))
  (product term-p 1 inc b))
  
  ; iterative
  
  (define (prod-proc-i a b)
  (define (term-p n)
  (if (= n 1)
      (/ 2 3)
      (/ 1 (- 1
              (/ 1.0 (* 4 (sq n)))))))
  (product-i term-p 1 inc b))
 
  
