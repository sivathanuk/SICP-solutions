#lang sicp

; base functions
(define (square n)
  (* n n))

(define (id n)
  n)

(define (divisible? p q)
  (= (remainder p q) 0))

(define (even? n)
  (= (remainder n 2) 0))

(define (avg x y)
  (/ (+ x y)
     2))

(define (close-enough x y)
  (< (abs (- x y)) 0.001))

(define (print-series func n)
  (define (print-s n i)
    (if (< i n)
        (begin
         (display (func i))
         (newline)
         (print-s n (+ i 1)))))
  (print-s n 0))

; exp r,i
(define (exp-r base pow)
  (if (= pow 0)
      1
      (* base (exp-r base (- pow 1)))))

(exp-r 3 4)

(define (exp-i base pow)
  (define (exp-iter pow result)
    (if (= pow 0)
        result
        (exp-iter (- pow 1) (* base result))))
  (exp-iter pow 1))

(exp-i 3 4)

; sum procedure r,i
(define (sigma-r func a next b)
  (if (> a b)
      0
      (+ (func a) (sigma-r func (next a) next b))))

(sigma-r square 1 inc 4)
(sigma-r id 1 inc 3)

; sigma-i
(define (sigma-i func a next b)
  (define (sigma-iter a result)
    (if (> a b)
        result
        (sigma-iter (next a) (+ (func a)
                                result))))
  (sigma-iter a 0))

(sigma-i square 1 inc 4)
(sigma-i id 1 inc 3)

; primality divisor
(define (prime? n)
  (= (smallest-divisor n 2) n))

(define (smallest-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divisible? n test-divisor) test-divisor)
    ((even? test-divisor)
     (smallest-divisor n (+ test-divisor 1)))
    (else
     (smallest-divisor n (+ test-divisor 2)))))

(prime? 7)
(prime? 101)

; fibonacci r,i
(define (fib-r n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else
     (+ (fib-r (- n 1))
        (fib-r (- n 2))))))

(print-series fib-r 10)

(define (fib-i n)
  (define (fib-iter a b i)
    (if (> i n)
        a
        (fib-iter b (+ b a) (+ i 1))))
  (fib-iter 0 1 1))

(print-series fib-i 2)
                  
; sqrt normal
(define (sqrt n)
  (define (improve y)
    (avg y (/ n y)))
  (define (try y)
    (if (close-enough n (square y))
        y
        (try (improve y))))
  (try 1.0))

; half-interval method
(define (search f neg-point pos-point)
  (let
      ((mid-point (avg neg-point pos-point)))
    (if (close-enough neg-point pos-point)
        mid-point
        (let
            ((test-value (f mid-point)))
          (cond
            ((negative? test-value) (search f mid-point pos-point))
            ((positive? test-value) (search f neg-point mid-point))
            (else
             mid-point))))))

(define test-func (lambda (x)
                    (+ (square x)
                       (* -5 x)
                       -40)))
; graph, https://www.desmos.com/calculator/kreo2ssqj8
(search test-func 0.1 11)

(define (half-interval-method f a b)
  (let
      ((ya (f a))
       (yb (f b)))
    (cond
      ((and (negative? ya) (positive? yb))
       (search f a b))
      ((and (negative? yb) (positive? ya))
       (search f b a))
      (else
       (error "the points are of the same sign" ya yb)))))
; GCD
(define (gcd a b)
  (if (= (remainder a b) 0)
      b
      (gcd b (remainder a b))))

(gcd 21 72)
(gcd 24 56)

; primality Euler
(define (exp-mod base pow p)
  (cond
    ((= pow 0) 1)
    ((even? pow)
     (remainder (square (exp-mod base (/ pow 2) p))
                p))
    (else
     (remainder (* base (exp-mod base (- pow 1) p))
                p))))

(define (fermats-test p)
  (define (try a)
    (= (exp-mod a p p) a))
  (try (+ 1 (random (- p 1)))))

(define (fermat-check p times)
  (cond
    ((= times 0) #t)
    ((fermats-test p) (fermat-check p (- times 1)))
    (else #f)))


; integral cube- [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2)+...] * dx
; Simpsons Method cube
#| h/3 [y0 +  4 * y1 + 2 * y2 + 4 * y3 + 2 * y4 + ....+ 2 * y(n-2) + 4 * y(n-1) + yn]
h = (b-a)/ n 
yk = f(a + kh)|#


; cont-frac 1 r,i
; pi/4- (2 * 4 * 4 * 6 * 6 * 8....) / (3 * 3 * 5 * 5 * 7 * 7....)
; sin using Mclaurin

; fixed-point
; sqrt average damp
; sqrt Newton's
; repeat function

; repeat fixed-point average damp to find roots
; iterative improvement - sqrt. fixed-point

; cons dispatch, operator
; Church's numerals

#|list
indexing
length
append
last element
reverse
map
deep-reverse
fringe
|#






