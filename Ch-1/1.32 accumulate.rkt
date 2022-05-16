#lang sicp

; sum
(define (sum function a next b)
  (if (> a b)
      0
      (+ (function a) (sum function (next a) next b))))

; accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))


(define (cube x)
  (* x x x))

; sum
(accumulate + 0 cube 1 inc 2)
; 9

; product
(accumulate * 1 cube 1 inc 2)
; 8


; sum iterative

(define (sum-i term a next b)
  (define (sum-iter a result)
    (if (> a b) result
        (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

; accumulate iterative

(define (accumulate-i combiner null-value term a next b)
  (define (acc-iter a result)
    (if (> a b) result
        (acc-iter (next a) (combiner result (term a)))))
  (acc-iter a null-value))


; sum
(accumulate-i + 0 cube 1 inc 2)
; 9

; product
(accumulate-i * 1 cube 1 inc 2)
; 8



