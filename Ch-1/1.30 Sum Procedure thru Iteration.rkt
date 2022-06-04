; sum iter

(define (sum-i term a nxt b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (nxt a) (+ result
                         (term a)))))
  (iter a 0))
