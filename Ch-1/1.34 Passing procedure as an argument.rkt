(define (f g)
  (g 2))


(f sq)

(f (lambda (z)
     (* z (+ z 1))))
     
; 4
; 6

(f f)

; how it's executed
; >(f f)
; >(f 2)
; >(2 2)
; Error: 2 is not a procedure
