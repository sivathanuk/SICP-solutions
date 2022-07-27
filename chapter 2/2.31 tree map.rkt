#lang sicp

(define (square x)
  (* x x ))

(define t1 (list 1 3 (list 5 7 9) 11))

(define (square-tree tree)
  (tree-map square tree))

(define (tree-map proc test-tree)
  (cond
    ((null? test-tree) nil)
    ((not (list? (car test-tree)))
     (cons (proc (car test-tree))
           (tree-map proc (cdr test-tree))))
    (else
     (cons (tree-map proc (car test-tree))
           (tree-map proc (cdr test-tree))))))
