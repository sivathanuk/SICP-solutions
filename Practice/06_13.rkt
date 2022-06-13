#lang sicp

; helper functions

(define (average x y)
  (/ (+ x y)
     2))

(define (square n)
  (* n n))

(define (cube n)
  (* n (square n)))

(define (even? n)
  (= (remainder n 2) 0))

(define (divisible? n d)
  (= (remainder n d) 0))

(define tolerance 0.0001)

(define (close-enough a b)
  (< (abs (- a b)) tolerance))

(define (compose f g)
  (lambda (x)
    (f (g x))))




; exp r,i

(define (exp base pow)
  (if (= pow 0)
      1
      (* base (exp base (- pow 1)))))

(exp 4 4)

(define (exp-i base pow)
  (define (exp-iter pow prod)
    (if (= 0 pow)
        prod
        (exp-iter (- pow 1) (* prod base))))
  (exp-iter pow 1))

(exp-i 4 4)


; sum procedure r,i

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(sum (lambda (x) x) 1 inc 5)

(sum (lambda (x) (* x x)) 1 inc 5)


(define (sum-i term a next b)
  (define (sum-iter a sum)
    (if (> a b)
        sum
        (sum-iter (next a) (+ (term a) sum))))
  (sum-iter a 0))

(sum-i (lambda (x) x) 1 inc 5)
(sum-i (lambda (x) (* x x)) 1 inc 5)
; primality divisor

(define (prime? p)
  (display p)
  (= (smallest-divisor p 2) p))

(define (smallest-divisor p test-divisor)
  (cond
    ((> (square test-divisor) p) p)
    ((divisible? p test-divisor) test-divisor)
    (else
     (smallest-divisor p (+ test-divisor 1)))))

(prime? 7)
(prime? 12)
; fibonacci r,i

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 7)


(define (fib-i n)
  (define (fib-iter a b n)
    (if (= n 0)
        a
        (fib-iter b (+ a b) (- n 1))))
  (fib-iter 0 1 n))

(fib-i 7)

; sqrt normal

(define (sqrt n)
  
  (define (close-enough y)
    (< (abs (- (square y) n)) tolerance))
  
  (define (improve y)
    (average y (/ n y)))
  
  (define (try z)
    (if (close-enough z)
        z
        (try (improve z))))
  
  (try 1.0))

(sqrt 5)
(sqrt 121)

; half-interval method

(define (search f neg pos)
  (define midpoint (average neg pos))
  (if (close-enough neg pos)
      midpoint
        (let
            ((test-value (f midpoint)))
          (cond
            ((positive? test-value) (search f neg midpoint))
            ((negative? test-value) (search f midpoint pos))
            (else
             midpoint)))))

(define (half-interval-method f a b)
  (let
      ((ya (f a))
       (yb (f b)))
    (cond ((and (negative? ya) (positive? yb))
           (search f a b))
          ((and (negative? yb) (positive? ya))
           (search f b a))
          (else
           (error "the y-values are of the same sign" ya yb)))))

(half-interval-method sin 8.0 11.0)
       
        
; GCD
(define (gcd a b)
  (if (= (remainder a b) 0)
      b
      (gcd b (remainder a b))))

(gcd 21 35)
(gcd 88 24)

; primality Euler; (a^p mod p = a)

(define (exp-mod base pow p)
  (cond
    ((= pow 0) 1)
    ((even? pow) (remainder (square (exp-mod base (/ pow 2) p))
                            p))
    (else
     (remainder (* base
                   (exp-mod base (- pow 1) p))
                p))))

(exp-mod 5 7 7)

(define (fermats-test p)
  (define (try a)
    (= (exp-mod a p p) a))
  (try (+ 1
          (random (- p 1)))))

(define (fermats-prime? p count)
  (cond
    ((= count 0)
     (begin
       (display p)
       #t))
    ((fermats-test p) (fermats-prime? p (- count 1)))
    (else
     (begin
       (display p)
       #f))))

(fermats-prime? 101 15)
(fermats-prime? 111 15)


    
; integral cube- [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2)+...] * dx

(define dx 0.00001)

(define (integral f a b dx)

  (define t1 (+ a (/ dx 2)))

  (define next (lambda (x)
                 (+ x dx)))
  
  ( * (sum f t1 next b)
      dx))

(integral cube 0 1 dx)
                          
  
; Simpsons Method cube
#| h/3 [y0 +  4 * y1 + 2 * y2 + 4 * y3 + 2 * y4 + ....+ 2 * y(n-2) + 4 * y(n-1) + yn]
h = (b-a)/ n 
yk = f(a + kh)|#

(define (simpsons f a b n)

  (define h (/ (- b a) n))

  (define (coeff k)
    (cond
      ((or (= k 1) (= k n)) 1)
      ((even? k) 2)
      (else 4)))

  (define (y k)
    (f (+ a
          (* k h))))

  (define (term k)
    (* (coeff k)
       (y k)))

  (* (/ h 3.0)
     (sum term a inc n)))

(simpsons cube 0 1 1000)

; cont-frac 1 r,i

(define (cont-frac n d k)
  (define (sum-r i)
    (/ (n i)
       (+ (d i)
          (if (< i k)
              (sum-r (+ i 1))
              (/ (n k)
                 (d k))))))
  (sum-r 1))

(define uno (lambda (x) 1.0))

(cont-frac uno uno 100)

(define (cont-frac-i n d k)
  (define (sum-iter k temp-sum)
    (if (= k 0)
        temp-sum
        (sum-iter (- k 1) (/ (n k)
                             (+ (d k)
                                temp-sum)))))
  (sum-iter k 0))

(cont-frac-i uno uno 100)
              

   
; pi/4- (2 * 4 * 4 * 6 * 6 * 8....) / (3 * 3 * 5 * 5 * 7 * 7....)

(define (mul term a next b)
  (if (> a b) 1
      (* (term a) (mul term (next a) next b))))

(define (identity x)
  x)

(mul identity 1 inc 5)


(define (mul-i term a next b)
  (define (mul-iter a prod)
    (if (> a b)
        prod
        (mul-iter (next a) (* prod (term a)))))
  (mul-iter a 1))

(mul-i identity 1 inc 5)


(define (term i)
  (* (/ (* 2 i)
        (- (* 2 i) 1))
     (/ (* 2 i)
        (+ (* 2 i) 1))))

(define (pi-4 n)
  (* (mul term 1 inc n)
     0.5))

(* (pi-4 1000)
   4)


; fixed-point
(define (fixed-point f guess)
  (define (try g)
    (let
        ((next (f g)))
      (if (close-enough g next)
          g
          (try next))))
  (try guess))

(fixed-point cos 1.0)

; sqrt average damp
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (sqrt-damp n)
  (define (improve x)
    ((average-damp (lambda (y) (/ n y))) x))
  
  (define (try g)
    (if (close-enough n (square g))
        g
        (try (improve g))))
  (try 1.0))

(sqrt-damp 5)
(sqrt-damp 121)

; sqrt Newton's: deriv, newton's transform, fucntion => y^2 - x

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx)))

((deriv square) 5)

(define (newtons-transform f)
  (lambda (x)
    (- x
       (/ (f x)
          ((deriv f) x)))))

(define (sqrt-nm n)
  (fixed-point (newtons-transform (lambda (y) (- n (square y)))) 1.0))

(sqrt-nm 5)
(sqrt-nm 121)


; repeat function
(define (repeat f n)
  (if (= n 1)
      f
      (compose (repeat f (- n 1)) f)))


((repeat cos 10) 0.739)
((repeat square 2) 5)


(define (repeat-i f n)
  (define (iter n fin)
    (if (= n 1)
        fin
        (iter (- n 1) (compose fin f))))
    (iter n f))

((repeat-i cos 10) 0.739)
((repeat-i square 2) 5)

; repeat fixed-point average damp to find roots
; iterative improvement - sqrt. fixed-point






