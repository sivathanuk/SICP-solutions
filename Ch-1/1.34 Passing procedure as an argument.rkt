(define (f g)
  (g 2))


(f sq)

(f (lambda (z)
     (* z (+ z 1))))
     
; 4
; 6

(f f)

; application: not a procedure;
; expected a procedure that can be applied to arguments
; given: 2
