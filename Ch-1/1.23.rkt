#lang sicp

; prime divisibility test - odd only, runtime check

; prime?
(define (prime? n)
  (= (find-smallest-divisor n 2) n))

; find smallest divisor
(define (find-smallest-divisor n test-divisor)
  (cond ((< n (square test-divisor)) n)
        ((divisible? n test-divisor) test-divisor)
        ((= (remainder test-divisor 2) 0) (find-smallest-divisor n (+ test-divisor 1)))
        (else
         (find-smallest-divisor n (+ test-divisor 2)))))
         
                                                        
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
      (report-prime n (- (runtime) start-ts))))

; display the time taken
(define (report-prime n elapsed-time)
  (display n)
  (display " ***time-taken:")
  (display elapsed-time))


; ex 1.22 search for primes
(define (search-for-primes start end count)
  (if (and (< start end) (> count 0))
      (begin
      (cond ((even? start) (search-for-primes (+ start 1) end count))
            ((prime? start) (begin
                             (timed-prime-test start)
                             (newline)
                             (search-for-primes (+ start 2) end (- count 1))))
            (else
             (search-for-primes (+ start 1) end count))))))

; check
(search-for-primes 1000 10000 3)
(search-for-primes 10000 100000 3)
(search-for-primes 100000 1000000 3)
(search-for-primes 1000000 10000000 3)
(search-for-primes 10000000 100000000 3)
(search-for-primes 100000000 1000000000 3)
(search-for-primes 1000000000 10000000000 3)
(search-for-primes 10000000000 100000000000 3)







