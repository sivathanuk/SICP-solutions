#lang sicp

#|
  Fill in the missing expressions in the following procedures for computing the other matrix operations. (The  procedure accumulate-n is defined in exercise 2.36.)    
|#

(define m0 (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)))
(define v0 (list 10 11 12))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (accumulate-n op initial seq)
  (if (null? (car seq))
      nil
      (cons
       (accumulate op initial (map (lambda (x) (car x)) seq))
       (accumulate-n op initial (map (lambda (x) (cdr x)) seq)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))



 (define (matrix-*-matrix m n)
   (let ((cols (transpose n)))
     (map (lambda (x)
            (