#lang sicp

; square list 
(define (square-list items)
  (if (null? items)
      nil
      (let
          ((x (car items)))
        (cons (* x x)
              (square-list (cdr items))))))

; check
(square-list (list 1 2 3 4))

;map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;square-list using map 
(define (square-list-map items)
  (map (lambda (x) (* x x )) items))

;check
(square-list-map (list 1 2 3 4))