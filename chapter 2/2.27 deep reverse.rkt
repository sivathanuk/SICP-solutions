#lang sicp

; deep-reverse 
(define (deep-reverse lst)
  (define (sub lst temp)
    (cond
      ((null? lst) nil)
      ((pair? (car lst))
       (append (sub (cdr lst) temp) (list (deep-reverse (car lst)))))
      (else
       (append (sub (cdr lst) temp) (list (car lst))))))
  (sub lst nil))

; base functions
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (reverse lst)
  (define (rev-sub lst temp)
    (if (null? lst)
        nil
        (append (rev-sub (cdr lst) temp) (list (car lst)))))
  (rev-sub lst nil))

(define t (list 1 2 3 (list 4 5) (list 6 7 8)))
t
(deep-reverse t)



