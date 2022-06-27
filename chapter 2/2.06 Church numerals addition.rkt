#lang sicp

; zero
(define zero (lambda (f)
               (lambda (x) x)))

; check zero (identity function)
((zero cos) 2.3)

; add-1
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

; evaluation using substitution 

#|
one => (add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f x)))

two => (add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

three => (add-1 two)
(lambda (f) (lambda (x) (f ((two f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f (f x))) x))))
(lambda (f) (lambda (x) (f (f (f x)))))

inference: one = (f x), two = (f (f x)), three = (f (f (f x)))
|#

; plus function 
(define (plus a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; check plus function, f => square

(define (square n)
  (* n n))

(((plus
   zero
   (add-1 zero))
  square)
 2)

;(f 2) => 4

(((plus
   (add-1 zero)
   (add-1 (add-1 zero)))
  square)
 2)

; (f (f (f 2))) => 256 


; extra: understanding add-1, functions order and evaluation

#|
(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

(define (dbl f)
  (lambda (x)
    (f (f x))))

(define (square n)
  (* n n))

; check
(((add-1 dbl) square) 3)

(square ((dbl square) 3))

(square 81)

|#



