#lang sicp


(define (iterative-improve good-enough improve)
  (lambda (x)
    (if (good-enough x)
        x
        ((iterative-improve good-enough improve)
         (improve x)))))
          

(define (average x y)
  (/ (+ x y)
     2))

(define (square n)
  (* n n))

; sqrt 
(define (sqrt n)
  (define (good-enough y)
    (< (abs (- n (square y))) 0.0001))
  (define (improve y)
    (average y (/ n y)))
  ((iterative-improve good-enough improve)
   (improve 1.0)))

; fixed-point

(define (fixed-point f guess)
  (define (good-enough y)
    (< (abs (- y (f y))) 0.0001))
  (define (improve y)
    (f y))
  ((iterative-improve good-enough improve)
   (improve guess)))


  
    
