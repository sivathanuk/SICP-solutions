#lang sicp

; for-each 
(define (for-each proc items)
  (if (null? items)
      (newline)
      (begin
        (display (proc (car items)))
        (newline)
        (for-each proc (cdr items)))))

; check
(for-each (lambda (x) (- 1 x)) (list 1 2 3 4))

; map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; check 
(map (lambda (x) (- 1 x)) (list 1 2 3 4))
