#lang sicp

; support functions

(define (sq x) (* x x))

(define (divides a n)
  (= (remainder a n) 0))

(define (find-divisor num count)
  (cond ((> (sq count) num) num)
        ((divides num count) count)
        (else (find-divisor num
                                (+ count 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; largest divisor θ(1)

(define (largest-divisor n)
  (/ n (smallest-divisor n)))

; largest divisor θ(log n)

(define (find-divisor-large num count)
  (cond ((divides num count) count)
        (else (find-divisor-large num (- count 1)))))

(define (largest-divisor2 n)
  (find-divisor-large n (- n 1)))

         

