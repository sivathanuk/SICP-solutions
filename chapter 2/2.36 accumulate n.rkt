#lang sicp

; accumulate
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

; accumulate-n
(define m0 (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)
                 (list 10 11 12)))

(define (accumulate-n op init seq)
  (if (null? (car seq))
      nil
      (cons
       (accumulate op init (map (lambda (x) (car x)) seq))
       (accumulate-n op init (map (lambda (x) (cdr x)) seq)))))