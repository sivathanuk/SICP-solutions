#lang sicp

; make-interval
(define (make-interval a b)
  (cons a b))

; mul-interval
(define (mul-interval x y)
  (let
      ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; bounds
(define (lower-bound x)
  (min (car x) (cdr x)))

(define (upper-bound x)
  (max (car x) (cdr x)))

; div-interval

(define (sign i)
  (if (>= i 0)
      1
      -1))

(define (div-interval x y)
  (if (= (* (sign (lower-bound y)) (sign (upper-bound y)))
         -1)
      (error "interval spans 0")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

