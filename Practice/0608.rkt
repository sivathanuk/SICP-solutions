#lang sicp

; exp r,i
;r
(define (exp-r base pow)
  (if (= pow 0)
      1
      (* base (exp-r base (- pow 1)))))

(exp-r 2 5)

;i
(define (exp-i base pow)
  (define (exp-iter base pow prod)
    (if (= pow 0)
        prod
        (exp-iter base (- pow 1) (* prod base))))
  (exp-iter base pow 1))

(exp-i 2 5)


; sum procedure r,i
;r

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(sum (lambda (x) x) 1 inc 5)

;i
(define (sum-i term a next b)
  (define (sum-iter term a next b agg)
    (if (> a b) agg
        (sum-iter term (next a) next b (+ agg (term a)))))
  (sum-iter term a next b 0))

(sum-i (lambda (x) x) 1 inc 5)
; primality divisor

(define (square n)
  (* n n))

(define (is-even n)
  (= (remainder n 2) 0))

(define (is-divisible n d)
  (= (remainder n d) 0))

(define (is-prime p)
  (= (smallest-divisor p 2) p))

(define (smallest-divisor p test-divisor)
  (cond ((> (square test-divisor) p) p)
        ((is-divisible p test-divisor) test-divisor)
        ((is-even test-divisor) (smallest-divisor p (+ test-divisor 1)))
        (else
         (smallest-divisor p (+ test-divisor 2)))))

(is-prime 7)
(is-prime 70)
; fibonacci r,i
;r

(define (fib n)
  (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 2)
(fib 5)

(define (fib-i n)
  (define (fib-iter a b n)
    (if (= n 0)
        a
        (fib-iter b (+ a b) (- n 1))))
  (fib-iter 0 1 n))

(fib-i 2)
(fib-i 5)
; sqrt normal

(define (average x y)
  (/ (+ x y)
     2))


(define (sqrt x)

  (define (improve g)
    (average g (/ x g)))

  (define (good-enough g)
    (< (abs (- (square g) x)) 0.0001))

  (define (guess g)
      (if (good-enough g)
      g
      (guess (improve g))))
  (guess 1.0))
    
(sqrt 5)
(sqrt 121)

         
; half-interval method


; GCD
(define (gcd a b)
  (if (= (remainder a b) 0)
      b
      (gcd b (remainder a b))))

(gcd 21 35)

; primality Euler (a^p mod p = a)

(define (exp-mod base pow p)
  (cond ((= pow 0) 1)
        ((is-even pow)
         (remainder (square (exp-mod base (/ pow 2) p))
                    p))
         (else
          (remainder (* base
                        (exp-mod base (- pow 1) p))
                     p))))
                     
(exp-mod 2 7 7)



(define (fermats-test p)
  (define (try n)
    (if (= (exp-mod n p p) n)
        #t
        #f))
  (try (+ 1
          (random (- p 1)))))

(fermats-test 7)
(fermats-test 111)

(define (is-fermats-prime p times)
  (cond ((= times 0) #t)
        ((fermats-test p) (is-fermats-prime p (- times 1)))
        (else #f)))

(is-fermats-prime 23 15)
(is-fermats-prime 10 15)

; integral cube- [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2)+...] * dx

(define (integral f a b)
  (define dx 0.0001)
  (* (sum f (+ a (/ dx 2)) (lambda (y) (+ y dx)) b)
     dx))

(define (cube n)
  (* n (square n)))

(integral cube 0 1)
       


                  
; Simpsons Method cube
#| h/3 [y0 +  4 * y1 + 2 * y2 + 4 * y3 + 2 * y4 + ....+ 2 * y(n-2) + 4 * y(n-1) + yn]
h = (b-a)/ n 
yk = f(a + kh)|#

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  
  (define (coeff k)
    (cond ((or (= k 0) (= k n))
           1)
          ((is-even k) 2)
          (else 4)))

  (define (y k)
    (f (+ a
          (* k h))))

  (define (term k)
    (* (y k)
       (coeff k)))

  ( * (sum term a inc n)
      (/ h 3)))

  
(simpsons cube 0 1.0 100)
         
  

; cont-frac 1 r,i
;r 
(define (cont-frac n d k)
  (define (sum i)
    (/ (n i)
       (+ (d i)
          (if (< i k)
              (sum (+ i 1))
              0))))
  (sum 1))

(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 10)



;i


(define (cont-frac-i n d k)
  (define (sum-iter agg i)
    (if (= i 0)
        agg
        (sum-iter (/ (n i)
                     (+ (d i)
                        agg))
                  (- i 1))))

  (sum-iter 0 k))

(cont-frac-i (lambda (x) 1.0) (lambda (x) 1.0) 100)


  
; pi/4- (2 * 4 * 4 * 6 * 6 * 8....) / (3 * 3 * 5 * 5 * 7 * 7....)

(define (mul term a next b)
  (if (> a b)
      1
      (* (term a)
         (mul term (next a) next b))))

(mul (lambda (x) x) 1 inc 5)

(define (s1 k)
  (/ (* 2 k)
     (- (* 2 k) 1)))

(define (s2 k)
  (/ (* 2 k)
     (+ (* 2 k)
        1)))

(define (term k)
  (* (s1 k)
     (s2 k)))

(* (/ 1 2)
   (mul term 1 inc 1000)
   4.0)
  
; fixed-point

(define (fixed-point f guess)
  (define (close-enough x y)
    (< (abs (- x y)) 0.0001))
  (define (try g)
    (let
        ((next (f g)))
    (if (close-enough g next)
        g
        (try next))))
  (try 1.0))

(fixed-point cos 1.0)
; sqrt average damp

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (sqrt-fp x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqrt-fp 3)

; sqrt Newton's

; f = x - y^2
; g => x - ( f(x) / (f'(x))

(define dx 0.00001)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
        (f x))
       dx)))

(define (newtons-transform f)
  (lambda (x)
    (- x (/ (f x)
            ((deriv f) x)))))

(define (sqrt-nm x)
  (fixed-point (newtons-transform (lambda (y) (- x (square y)))) 1.0))

(sqrt-nm 2)




; repeat function: r, i
; r

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose (repeat f (- n 1)) f)))

((repeat cos 10) 0.739)

; i

(define (repeat-i f n)
  (define (r-iter result k)
    (if (= k 0)
        result
        (r-iter (compose result f) (- k 1))))
  (r-iter f n))

((repeat-i cos 10) 0.739)

; repeat fixed-point average damp to find roots
; iterative improvement - sqrt. fixed-point


(define (iterative-improvement good-enough improve)
  (lambda (x)
    (if (good-enough x)
        x
        ((iterative-improvement good-enough improve)
         (improve x)))))

; sqrt

(define (sqrt-ii x)
  (define (good-enough guess)
    (< (abs (- x (square guess))) 0.0001))
  (define (improve y)
    (average y (/ x y)))
  ((iterative-improvement good-enough improve) 1.0))

(sqrt-ii 2)
(sqrt-ii 3)

; fixed-point

(define (fixed-point-ii f guess)
  (define (good-enough guess)
    (< (abs (- guess (f guess)))))
  (define (improve guess)
    (f guess))
  ((iterative-improvement good-enough improve) guess))

(fixed-point cos 1.0)
