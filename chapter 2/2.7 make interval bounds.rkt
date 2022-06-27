#lang sicp

(define (make-interval a b) (cons a b))

(define test (make-interval 5 20))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (lower-bound x)
  (min (car x) (cdr x)))
