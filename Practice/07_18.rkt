#lang sicp

; list
(define odds (list 1 3 5 7 9 11))
(define squares (list 1 4 9 16 25))

;indexing
(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))
odds
(display "list-ref odds 3: ")
(list-ref odds 3)

;length
(define (len-r lst)
  (if (null? lst)
      0
      (+ 1 (len-r (cdr lst)))))
odds
(display "len-r odds: ")
(len-r odds)

(define (len-i lst)
  (define (len-iter lst result)
    (if (null? lst)
        result
        (len-iter (cdr lst) (+ 1 result))))
  (len-iter lst 0))

odds
(display "len-i squares ")
(len-i squares)

;append
(define (append-r list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append-r (cdr list1) list2))))

(display "append-r odds squares")
(append-r odds squares)

;last element
(define (last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (last-element (cdr lst))))
odds
(display "last element odds: " )
(last-element odds)

;reverse
(define (reverse lst)
  (define (reverse-loop lst result)
    (if (null? lst)
        result
        (reverse-loop (cdr lst) (append (list (car lst)) result))))
  (reverse-loop lst nil))

odds
(display "reverse odds: ")
(reverse odds)

;map
(define (map proc lst)
  (define (map-sub lst result)
    (if (null? lst)
        result
        (map-sub (cdr lst) (append result (list (proc (car lst)))))))
  (map-sub lst nil))


(define (square x) (* x x))
odds
(display "map square odds: ")
(map square odds)

;deep-reverse
(define (deep-reverse lst)
  (define (dr lst result)
    (cond
      ((null? lst) result)
      ((not (list? (car lst)))
       (dr (cdr lst) (append (list (car lst)) result)))
      (else
       (dr (cdr lst) (append (list (deep-reverse (car lst))) result)))))
  (dr lst nil))

(define t1 (list 1 (list 2 3) 4 (list 5 6 7) 8))
t1
(display "deep-reverse: ")
(deep-reverse t1)
  
;fringe
(define (fringe lst)
  (define (fringe-sub lst result)
    (if (null? lst)
        result
        (let
            ((head (car lst))
             (tail (cdr lst)))
          (if (not (list? head))
              (fringe-sub tail (append result (list head)))
              (fringe-sub tail (append result (fringe head) ))))))
  (fringe-sub lst nil))



t1
(display "fringe: ")
(fringe t1)

         



