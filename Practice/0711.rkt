#lang sicp

; list-
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9))

; indexing
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref odds 3)
(list-ref squares 2)

; length
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (len-i lst)
  (define (iter lst s)
    (if (null? lst)
        s
        (iter (cdr lst) (+ s 1))))
  (iter lst 0))

(length (cons 11 odds))
(len-i squares)

; append
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(append odds squares)
; last pair
(define (last items)
  (if (null? (cdr items))
      (car items)
      (last (cdr items))))

(last odds)
(last (list 1 8 27))

; reverse

; using append
(define (reverse items)
  (define (rev-sub items l)
    (if (null? items)
        l
        (rev-sub (cdr items) (append (list (car items)) l))))
  (rev-sub items (list )))


  
 
             