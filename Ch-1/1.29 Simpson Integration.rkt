; simpson's rule

 (define (is-even n)
  (= (remainder n 2) 0))



(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(define (simpsons fun a b n)
  
  (define h (/ (- b a) n))

  (define (coeff k)
    (cond ((or (= 0 k)
               (= n k)) 1)
          ((is-even k) 2)
          (else 4)))

  (define (yk k)
    (* (coeff k)
       (fun (+ a
               (* k h)))))
  (* (sum yk a inc n)
     ( / h 3.0)))
