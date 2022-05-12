; function: *
(define (* a b)
  (if (= b 0) 0
      (+ a (* a
              (- b 1)))))

; function: double
(define (double x) (+ x x))

; function: half
(define (half x) (/ x 2))

; function: is-even
(define (is-even x) ( = (remainder x 2) 0))

; function: fast-mul
(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((is-even b) (double (fast-mul a (half b))))
        (else (+ a (double (fast-mul a (half (- b 1))))))))

; compare runtime
(define start1 (runtime))
(* 12324 98999)
(define end1 (runtime))
(display "runtime1:")
(- end1 start1)

(define start2 (runtime))
(fast-mul 12324 98999)
(define end2 (runtime))
(display "runtime2:")
(- end2 start2)

; OUTPUT:

; 1220063676
; 30106
; 1220063676
; 202
