#lang sicp

; primality Fermat's: a^p is congruent to a (mod p)
(define (expmod base pow p)
  (cond ((= pow 0) 1)
        ((is-even pow)
         (remainder (sq (expmod
                         base
                         (/ pow 2)
                         p))
                    p))
        (else
         (remainder (* base
                       (expmod base
                               (- pow 1)
                               p))
                    p))))

(define (fermat-test p)
  (define (try-it base)
    (= (expmod base p p) base))
  (try-it (+ 1 (random (- p 1)))))

(define (is-fermat-prime p freq)
  (cond ((= freq 0) #t)
        ((fermat-test p)
         (is-fermat-prime p
                          (- freq 1)))))


; square
(define (sq n)
  (* n n ))
   
; even?
(define (is-even n)
  (= (remainder n 2) 0))

; check Carmichael numbers : 561, 1105, 1729, 2465, 2821, 6601

(is-fermat-prime 561 25)
(is-fermat-prime 1105 25)
(is-fermat-prime 1729 25)
(is-fermat-prime 2465 25)
(is-fermat-prime 2821 25)
(is-fermat-prime 6601 25)
