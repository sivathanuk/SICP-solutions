#lang sicp 

(define (cons a b)
  (* (exp 2 a)
     (exp 3 b)))
     

(define (car p)
  (define (count p x)
    (if (not (= (remainder p 2) 0))
        (display x)
        (count (quotient p 2) (+ x 1))))
  (count p 0))
    

(define (cdr p)
  (define (count p x)
    (if (= (remainder p 3) 0)
        (count (quotient p 3) (+ x 1))
        x))
  (count p 0))
  
; check 

(cons 10 8)
(car 6718464)
(cdr 6718464)
