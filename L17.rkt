#lang plai
(define counter 0)
(define  (h x)
  (begin
    (set! counter (+ x counter))
    counter))

(h 2)
(h 2)


(define g
  (local [(define b (box 0))]
    (lambda (x)
      (begin
        (set-box! b (+ x (unbox b)))
        (unbox b)))))
(g 5)
(g 5)
(g 5)


(define (f x)
  (+ x (read)))

(f 5)



