# lang sicp

; constructors and selectors
; point
(define (make-point x y)
  (cons x y))

(define (x-point z)
  (car z))

(define (y-point z)
  (cdr z))

(define (print-point z)
  (newline)
  (display "(")
  (display (x-point z))
  (display ",")
  (display (y-point z))
  (display ")"))
  
;--------------------------------------------;
; rectangle 1 (given 4 points)
(define (rect1 p1 p2 p3 p4)
  (cons (cons p1 p2)
        (cons p3 p4)))
        
;--------------------------------------------;      
; helper functions
(define (square n)
  (* n n))

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough x y)
  (< (abs (- x y)) 0.0001))

(define (sqrt x)
  (define (improve y)
    (average y (/ x y)))
  (define (try guess)
    (if (close-enough (square guess) x)
        guess
        (try (improve guess))))
  (try 1.0))
  
; geometric center
(define (geom-center a b c d)
  (make-point (/ (+ (x-point a) (x-point b) (x-point c) (x-point d)) 4.0)
              (/ (+ (y-point a) (y-point b) (y-point c) (y-point d)) 4.0)))
              
;--------------------------------------------;
; distance between two points
(define (distance p1 p2)
  (let
      ((x1 (x-point p1))
       (x2 (x-point p2))
       (y1 (y-point p1))
       (y2 (y-point p2)))
    (sqrt
     (+ (square (- x2 x1))
        (square (- y2 y1))))))
        
; is-rectangle ?
(define (rectangle? rect)
  (let
      ((p1 (car (car rect)))
       (p2 (cdr (car rect)))
       (p3 (car (cdr rect)))
       (p4 (cdr (cdr rect))))
    (let
        ((c (geom-center p1 p2 p3 p4)))
      (if (= (round (distance p1 c))
             (round (distance p2 c))
             (round (distance p3 c))
             (round (distance p4 c)))
          #t
          #f))))
        
; rectangle 1 area, perimeter
(define (area1 rect)
   (let
      ((seg1 (distance (car (car rect)) (cdr (car rect))))
       (seg2 (distance (cdr (car rect)) (car (cdr rect))))
       (seg3 (distance (car (cdr rect)) (cdr (cdr rect))))
       (seg4 (distance (cdr (cdr rect)) (car (car rect)))))
     (sqrt (* seg1 seg2 seg3 seg4))))

(define (perimeter1 rect)
  (let
      ((seg1 (distance (car (car rect)) (cdr (car rect))))
       (seg2 (distance (cdr (car rect)) (car (cdr rect))))
       (seg3 (distance (car (cdr rect)) (cdr (cdr rect))))
       (seg4 (distance (cdr (cdr rect)) (car (car rect)))))
    (+ seg1 seg2 seg3 seg4)))
   
;--------------------------------------------;
;check output rectangle 1
; provide points
(define p1 (make-point 10 70))
(define p2 (make-point 10 20))
(define p3 (make-point 40 20))
(define p4 (make-point 40 70))


(define rect-4p (rect1 p1 p2 p3 p4))

(rectangle? rect-4p)

(area1 rect-4p)
(perimeter1 rect-4p)

#| output
#t
1500.000003815791
160.00000025438595
|#
