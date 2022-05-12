; T^2 (a,b) => 
; p' = p^2 + q^2 
; q' = q^2 + 2pq

(define (is-even n)
  (= (remainder n 2) 0))

(define (fib-lr n)
  (fib-logiter 1 0 0 1 n))

(define (fib-logiter a b p q count)
  (cond ((= count 0) b)
   ((is-even count)
    (fib-logiter a
              b
              (+ (* p p)
                 (* q q))
              (+ (* q q)
                 (* 2 p q))
              (/ count 2)))
   (else (fib-logiter (+ (* b q)
                      (* a q)
                      (* a p))
                   (+ (* b p )
                      (* a q))
                   p
                   q
                   (- count 1)))))
