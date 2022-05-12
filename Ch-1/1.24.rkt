#lang sicp
; using fast prime (fermats little theorem)
; Fermat's test a^p congruent to  a (mod p)
;exp-mod

; search for primes fast
(define (search-for-primes-f start end counter)
  (if (and (> counter 0) (< start end))
      (cond ((is-even start) (search-for-primes-f (+ start 1) end counter))
            ((fast-prime start 25)
             (timed-prime-test-f start) (search-for-primes-f (+ start 2) end (- counter 1)))
            (else
             (search-for-primes-f (+ start 2) end counter)))))

; is-even
(define (is-even n)
  (= (remainder n 2) 0))

; timed prime test
(define (timed-prime-test-f n)
  (display n)
  (start-prime-test-f n (runtime)))

(define (start-prime-test-f n start-time)
  (if (fast-prime n 20)
       (report-prime-f (- (runtime) start-time)))
  false)

(define (report-prime-f elapsed-time)
  (display " ***")
  (display elapsed-time)
  (display " ")
  (newline)
  true)


; expmod
(define (exp-mod base exp p)
  (cond ((= exp 0) 1)
        ((is-even exp)
         (remainder (sq (exp-mod base (/ exp 2) p))
                                  p))
        (else
         (remainder (* base (exp-mod base (- exp 1) p))
                         p))))

; Fermat's test
(define (fermats-test n)
  (define (try-it a)
    (= (exp-mod a n n) a))
    (try-it (+ 1 (random (- n 1)))))


; fast prime
(define (fast-prime n times)
  (cond ((= times 0) true)
        ((fermats-test n) (fast-prime n (- times 1)))
        (else false)))

; square
(define (sq n)
  (* n n ))

(display "time taken for fast prime:")
(newline)
(search-for-primes-f 1000 10000 3) ; a
(search-for-primes-f 10000 100000 3) ; b
(search-for-primes-f 100000 1000000 3) ; b
(search-for-primes-f 1000000 10000000 3)



