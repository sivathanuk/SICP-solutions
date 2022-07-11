#lang sicp

(define (count-change amount)
  (cc amount 5))

(define (cc amount n)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (= n 0)) 0)
    (else
     (+ (cc amount (- n 1))
        (cc (- amount
               (denom n))
            n)))))

(define (denom n)
  (cond
    ((= n 5) 50)
    ((= n 4) 25)
    ((= n 3) 10)
    ((= n 2) 5)
    (else 1)))

; using list
(define (cc-list amount coin-values)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (nomore? coin-values)) 0)
    (else
     (+ (cc-list amount
                 (except-first-denomination coin-values))
        (cc-list (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (except-first-denomination lst)
  (cdr lst))

(define (first-denomination lst)
  (car lst))

(define (nomore? lst)
  (if (null? lst)
      #t
      #f))

(count-change 100)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc-list 100 us-coins)
     