#lang sicp
; constructors and selectors
(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

; helper functions
(define (average x y)
  (/ (+ x y) 2.0))

; midpoint 
(define (midpoint-segment l)
  (let
      ((p1 (car l))
       (p2 (cdr l)))
    (let
        ((x-mid (average (x-point p1) (x-point p2)))
         (y-mid (average (y-point p1) (y-point p2))))
      (define midpoint (make-point x-mid y-mid))
      midpoint)))

; print point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; print start, end and midpoint
(define (print-mid l)
  (newline)
  (display "start:")
  (print-point (car l))
  (newline)
  (display "end:")
  (print-point (cdr l))
  (newline)
  (display "midpoint =>")
  (print-point (midpoint-segment l)))


;check 
(define start (make-point 5 10))
(define end (make-point 15 30))
(define l1 (make-segment start end))

(midpoint-segment l1)

(print-mid l1)

#| output:
(10.0 . 20.0)

start: 
(5, 10)
end: 
(15, 30)
midpoint => 
(10.0, 20.0)
|#



  

  
      
