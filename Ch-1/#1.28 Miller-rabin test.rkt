#lang sicp

(define (expmod base pow p)
  (cond ((= pow 0) 1)
        ((is-even pow)
         (remainder (sq (expmod base (/ pow 2) p))
                    p))
        (else
         (remainder (* base (expmod base (- pow 1) p))
                    p))))

(define (non-trivial n p)
  (if (and (not (or (= n 1)
                    (= n (- p 1))))
       (= (remainder (sq n)
                     p)
          1))
      n
      0))

(define (sq n)
  (* n n))

(define (is-even n)
  (= (remainder n 2) 0))

; fermat's test

(define (fermat-test p)
  (define (try-it a)
    (= (expmod a p p) a))
  (try-it (+ 1 (random (- p 1)))))

;miller-rabin

(define (miller-rabin p)
  (define (try-it a)
    (if (= (expmod (non-trivial a p)
                        p
                        p)
                a)
        #t
        #f))
  (try-it (+ 1 (random (- p 1)))))


(define (algo-check p)
  (display p)
  (newline)
  (display " fermat-test:")
  (display (fermat-test p))
  (newline)
  (display " miller-rabin-test:")
  (display(miller-rabin p))
  (newline))

(algo-check 561)
(algo-check 1105)
(algo-check 1729)
(algo-check 2465)
(algo-check 2821)
(algo-check 6601)
(algo-check 321197185)




            
           
        
     