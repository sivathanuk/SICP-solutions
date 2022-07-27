#lang sicp

(define t1 (list 1 2 (list 3 4 5) (list 6 7)))

(define (square x)
  (* x x))

(define (square-tree test-tree)
  (cond
    ((null? test-tree) nil)
    ((not (list? (car test-tree)))
     (cons (square (car test-tree))
           (square-tree (cdr test-tree))))
    (else
     (cons (square-tree (car test-tree))
           (square-tree (cdr test-tree))))))

(define (square-tree-m test-tree)
  (map (lambda (sub-tree)
         (if (not (list? sub-tree))
             (square sub-tree)
             (square-tree-m sub-tree)))
       test-tree))