#lang sicp

(define t (list  (list 1 2) (list 3 4 5) 6 7 8 (list 9 10)))



(define (fringe items)
  (cond
    ((null? items) nil)
    ((not (list? (car items)))
     (cons (car items) (fringe (cdr items))))
    (else
     (append (fringe (car items)) (fringe (cdr items))))))


(define x (list (list 1 2) (list 3 4)))

; fringe 2

(define (fringe2 items)
  (cond
    ((null? items) nil)
    ((not (list? (car items)))
     (cons (car items) (fringe2 (cdr items))))
    (else
     (append (fringe2 (car items)) (fringe2 (cdr items))))))
; iteration
       
