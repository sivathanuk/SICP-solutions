#lang sicp

; prime divisibility test, runtime check

; prime?
(define (prime? n)
  (= (find-smallest-divisor n 2) n))

; find smallest divisor
(define (find-smallest-divisor n test-divisor)
  (cond ((< n (square test-divisor)) n)
        ((divisible? n test-divisor) test-divisor)
        (else
         (find-smallest-divisor n (+ test-divisor 1)))))

; square
(define (square n)
  (* n n ))

; divisible?
(define (divisible? n test)
  (= (remainder n test) 0))


; runtime check
; get start time
(define (timed-prime-test n)
  (start-test n (runtime)))

; find (end-time - start-time)
(define (start-test n start-ts)
  (if (prime? n)
      (report-prime n (- (runtime) start-ts))
      #f))

; display the time taken
(define (report-prime n elapsed-time)
  (display n)
  (display " ***time-taken:")
  (display elapsed-time)
  #t)


; ex 1.22 search for primes

;is-even
(define (is-even n)
  (= (remainder n 2) 0))

;search for primes
(define (search-for-primes start end count)
  (if (and (< start end) (> count 0))
      (cond ((timed-prime-test start) (begin
                                        (newline)
                                        (search-for-primes (+ start 2) end (- count 1))))
            ((is-even start) (search-for-primes (+ start 1) end count))
            (else
             (search-for-primes (+ start 2) end count)))))

(search-for-primes 10000 100000 3)
(search-for-primes 1000000 10000000 3)
(search-for-primes 100000000 1000000000 3)
(search-for-primes 1000000000 10000000000 3)
(search-for-primes 100000000000 1000000000000 3)






