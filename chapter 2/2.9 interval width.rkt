#lang sicp

; width
(define (width x)
  (* 0.5 (- (upper-bound x)
            (lower-bound x))))

(define (width-interval a b)
  (+ (width a)
     (width b)))

; basic functions
(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (lower-bound x)
  (min (car x) (cdr x)))

; operations
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(define (mul-interval x y)
  (let
      ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))



; initiate intervals
(define a (make-interval 2 3))
(define b (make-interval 1 5))

(display "intervals: a, b")
(newline)
a b

; addition
(display "addition")
(add-interval a b)
(width (add-interval a b))
(width-interval a b)

; subtract interval
(display "subtraction")
(sub-interval a b)
(width (sub-interval a b))
(width-interval a b)

; mul interval
(display "multiplication")
(mul-interval a b)
(width (mul-interval a b))
(width-interval a b)

; division
(display "division")
(div-interval a b)
(width (div-interval a b))
(width-interval a b)

; output

#|
intervals: a, b
(2 . 3)
(1 . 5)
addition(3 . 8)
2.5
2.5
subtraction(-3 . 2)
2.5
2.5
multiplication(2 . 15)
6.5
2.5
division(0.4 . 3.0)
1.3
2.5

|#
