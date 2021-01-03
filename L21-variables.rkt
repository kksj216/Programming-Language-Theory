#lang plai
; L21 - variable
(define counter 0)
(define (f x)
  (begin
    (set! counter
          (+ x counter))

    counter))
(f 10) 

(define count (box 0))
(define (g x)
  (begin
    (set-box! count
              (+ (unbox x) 
                   (unbox count)))
              (unbox count)))
(g (box 10)) 