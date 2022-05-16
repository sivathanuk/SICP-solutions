#lang sicp

; filter 

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))


(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          (combiner null-value (filtered-accumulate filter combiner null-value term (next a) next b)))))


(define (is-even n)
  (= (remainder n 2) 0))

(define (cube n)
  (* n n n))

; even-filter
(filtered-accumulate is-even + 0 cube 1 inc 9)
