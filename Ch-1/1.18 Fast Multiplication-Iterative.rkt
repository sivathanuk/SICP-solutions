(define (mul-log-count a b)
  (mul-logiter a 0 b))
  
(define (mul-logiter a product b)
  (cond ((= b 0) product)
        ((is-even b) (mul-logiter (double a) product (/ b 2)))


