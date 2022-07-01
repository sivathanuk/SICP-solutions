#lang sicp

; base functions
(define (make-interval a b)
  (cons a b))

(define (lower-bound z)
  (min (car z) (cdr z)))

(define (upper-bound z)
  (max (car z) (cdr z)))
  
  
; center
(define (center i)
  (/ (+ (upper-bound i) (lower-bound i))
     2))

; width
(define (width i)
  (/ (- (upper-bound i) (lower-bound i))
     2))

; percentage
(define (percent i)
 (* (/ (width i) (center i) )
    100))

; interval to percentage
(define (interval-percentage-tol i)
  (make-interval (center i) (percent i)))

; percentage to interval
(define (interval-from-percent i)
  (let
      ((c (car i))
       (p (cdr i)))
  (make-interval (- c (* c p 0.01))
                 (+ c (* c p 0.01)))))
                 

; check 
(define z (make-interval 8 12))
z

(define p (interval-percentage-tol z))

p
(interval-from-percent p)

#| output:
(8 . 12)
(10 . 20)
(8.0 . 12.0)
|#
