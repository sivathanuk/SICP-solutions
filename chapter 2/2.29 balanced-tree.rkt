#lang sicp

(define odds (list 1 3 5 7))

; constructors
(define (make-branch length structure)
  (list length structure))

(define (make-mobile left right)
  (list left right))

; selectors
(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (car (cdr tree)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; sample tree
(define left (make-branch 100 6))

(define right (make-branch 10
                           (make-mobile (make-branch 20 40)
                                        (make-branch 30 20))))
(define mobile (make-mobile left right))

(define right2 (make-branch 10
                           (make-mobile (make-branch 20 40)
                                        (make-branch 40 20))))
(define mobile2 (make-mobile left right2))




; weight
; total-weight
(define (weight-tree tree)
  (+ (weight (left-branch tree))
     (weight (right-branch tree))))

; w
(define (weight branch)
  (let
      ((b-struct (branch-structure branch)))
    (cond
      ((null? b-struct) 0)
      ((not (list? b-struct)) b-struct)
      (else
       (weight-tree b-struct)))))

; torque
(define (torque branch)
  (* (weight branch)
     (if (list? branch)
         (branch-length branch)
         1)))

; balanced
(define (balanced? tree)
  (let
      ((left (left-branch tree))
       (right (right-branch tree)))
    (and
      (= (torque left) (torque right))
      (if (list? (branch-structure left))
          (balanced? (branch-structure left))
          #t)
      (if (list? (branch-structure right))
          (balanced? (branch-structure right))
          #t))))
