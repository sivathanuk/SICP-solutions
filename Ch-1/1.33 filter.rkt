#lang sicp

; filtered-accumulate
(define (filtered-accumulate filter null-value combiner term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate filter null-value combiner term (next a) next b))
          (combiner null-value (filtered-accumulate filter null-value combiner term (next a) next b)))))

; sq
(define (sq n)
  (* n n))

; is-even
(define (is-even n)
  (= (remainder n 2) 0))

; is-prime
(define (is-prime p)
  (= (find-smallest-divisor p 2) p))


(define (find-smallest-divisor p n)
  (cond ((< p (sq n)) p)
        ((is-divisible p n) n)
        (else
         (find-smallest-divisor p (+ n 1)))))

(define (is-divisible p n)
  (= (remainder p n) 0))

; gcd
(define (gcd n d)
  (if (= (remainder n d) 0)
      d
      (gcd d (remainder n d))))

; identity
(define (identity x)
  x)
; a-prime addition

(filtered-accumulate is-prime 0 + sq 2 inc 5)


; b-relative-prime multiplication
(define (rel-prime-mul p)
  (define (rel-prime n)
    (= (gcd p n) 1))
  (filtered-accumulate rel-prime 1 * identity 1 inc p))




      
