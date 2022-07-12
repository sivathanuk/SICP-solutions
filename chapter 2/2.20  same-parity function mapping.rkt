#lang sicp

; same-parity function
(define (same-parity g . w)
  (if (even? g)
      (cons g (car (filter w)))
      (cons g (cdr (filter w)))))

; odd, even check and list creation
(define (filter inner-list)
  (define (filter-sub inner-list evens odds)
    (if (null? inner-list)
        (cons evens odds)
        (if (even? (car inner-list))
            (filter-sub (cdr inner-list) (append evens (list (car inner-list))) odds)
            (filter-sub (cdr inner-list) evens (append odds (list (car inner-list)))))))
  (filter-sub inner-list nil nil))

; base functions
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))
       
; check
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)




            
        



