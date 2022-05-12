#lang sicp


(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))


; gcd 206 40, need 4 iterations to meet b = 0 if condition

(gcd 206 40)
(gcd 40 (remainder 206 40))

; if check #1 - False, remainder execution (to check if b = 0) 1 time
(gcd (remainder 206 40)
     (remainder 40
                (remainder 206 40)))

; if check #2 - False, remainder execution 2 times: Total = 1 + 2 => 3 times
(gcd (remainder 40
                (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40))))

; if check #3 - False, remainder execution 4 times: Total = 1 + 2 +4 => 7 times
;(gcd b
;     (remainder a b))

(gcd  (remainder (remainder 206 40)
                 (remainder 40
                            (remainder 206 40)))
      (remainder (remainder 40
                            (remainder 206 40))
                 (remainder (remainder 206 40)
                            (remainder 40
                                       (remainder 206 40)))))
                
; if check #4 - True, remainder execution 7 times: Total =  1 + 2 + 4 + 7 => 14
; execute a             

(remainder (remainder 206 40)
                 (remainder 40
                            (remainder 206 40)))
   

; executing a; remainder execution 4 times: Total = 1 + 2 + 4 + 7 + 4 => 18 times

;; Applicative order of evaluation
(gcd 206 40)

(gcd 40 (remainder 206 40)) ; 1
(gcd 40 6)

(gcd 6 (remainder 40 6)) ; 1
(gcd 6 4)

(gcd 4 (remainder 6 4)) ; 1
(gcd 4 2)
(gcd 2 (remainder 4 2)) ; 1
(gcd 2 0) 2
; remainder execution 1 + 1 + 1 + => 4 times


