#lang sicp

; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))

(define t1 (list 1 3 (list 5 7) 9))
(define t2 (list (list 7)))
(define t3 (list 1
                 (list 2
                       (list 3
                             (list 4
                                   (list 5
                                         (list 6 7)))))))
; t1 (1 3 (5 7) 9)
(car (cdr (car (cdr (cdr t1)))))

; t2 ((7))
(car (car t2))

; t3 (car (cdr x)) 6 times
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr t3))))))))))))


; using repeat 
; compose f
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (retrieve y)
  (car (cdr y)))

((repeat retrieve 6) t3)



                                 
