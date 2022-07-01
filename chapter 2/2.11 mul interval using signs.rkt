#lang sicp

; base functions
(define (make-interval a b)
  (cons a b))

(define (upper-bound z)
  (max (car z) (cdr z)))

(define (lower-bound z)
  (min (car z) (cdr z)))

(define (mul-interval x y)
  (let
      ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; fetch sign
(define (sign q)
  (if (>= q 0)
      1
      -1))
      
; sign tag for interval pair 
(define (sign-tag x)
  (* (sign (lower-bound x))
     (sign (upper-bound x))))

; mul-interval using signs
(define (mul-interval-signed x y)
  (let
      ((lx (lower-bound x))
       (ux (upper-bound x))
       (ly (lower-bound y))
       (uy (upper-bound y))
       (sign-tag-x (sign-tag x))
       (sign-tag-y (sign-tag y)))
    (cond
      ((and (= sign-tag-x 1) (= sign-tag-y 1))
       (if (negative? lx)
       (if (negative? ly)
           (make-interval (* ux uy) (* lx ly))
           (make-interval (* lx uy) (* ux ly))))
       (if (negative? ly)
           (make-interval (* ux ly) (* lx uy))
           (make-interval (* lx ly) (* ux uy))))
      ((and (= sign-tag-x -1) (= sign-tag-y 1))
       (if (negative? ly)
           (make-interval (* ux ly) (* lx ly))
           (make-interval (* lx uy) (* ux uy))))
      ((and (= sign-tag-x 1) (= sign-tag-y -1))
       (if (negative? lx)
           (make-interval (* lx uy) (* lx ly))
           (make-interval (* ux ly) (* ux uy))))
      (else
       (make-interval (min (* lx uy) (* ux ly))
                      (max (* lx ly) (* ux uy)))))))

;check
(define i1 (make-interval 1 5))
(define i2 (make-interval -10 -4))
(define i3 (make-interval -4 1))

(mul-interval i1 i2)
(mul-interval-signed i1 i2)

(mul-interval i2 i3)
(mul-interval-signed i2 i3)

(mul-interval i1 i3)
(mul-interval-signed i1 i3)

#| output:
(-50 . -4)
(-50 . -4)
(-10 . 40)
(-10 . 40)
(-20 . 5)
(-20 . 5)
|#
    
;---end--     
     
; extra to get all signs in a single object
;(define (mul-interval-signed x y)
;  (mul x y (get-signs x y)))


#|(define (get-signs x y)
  (let
      ((lx (lower-bound x))
       (ux (upper-bound x))
       (ly (lower-bound y))
       (uy (upper-bound y)))
    (make-interval (make-interval (sign lx) (sign ux))
                   (make-interval (sign ly) (sign uy)))))
|#

