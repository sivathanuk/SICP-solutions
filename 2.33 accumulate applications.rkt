#lang sicp

#|
(define (map p sequence)  (accumulate (lambda (x y) <??>) nil sequence))  (define (append seq1 seq2)  (accumulate cons <??> <??>))
(define (length sequence)  (accumulate <??> 0 sequence))
|#


; accumulate op initial sequence
(define (accumulate op inital sequence)
  (if (null? sequence)
      inital
      (op (car sequence)
          (accumulate op inital (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))







