#lang sicp

(define (square x)
  (* x x))

(define odds (list 1 3 5 7 9))

; first attempt
(define (square-list items)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items) (cons (square (car items))
                                answer))))
  (iter items nil))

; check 
(square-list odds)
(81 49 25 9 1)


; interchange arguments to cons
(define (square-list2 items)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items) (cons answer (square (car items))))))
  (iter items nil))

; check 
(square-list2 odds)

; (((((() . 1) . 9) . 25) . 49) . 81)


; using append

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (sq-list items)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items)
              (append answer (list (square (car items)))))))
  (iter items nil))

; check 
(sq-list odds)

; (1 9 25 49 81)

