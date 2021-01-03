#lang racket
;lecture 26

(define retry #f)

;(define factorial
;  (lambda (x)
;    (if (= x 0)
;        (call/cc (lambda (k) (set! retry k) 1))
;        (* x (factorial (- x 1))))))

;(factorial 4)
;(retry 1)
;(retry 2)

(+ (* 2 3) 10)  ;16

(+ (* (call/cc
       (lambda (k)
         (k 2))) 3) 10)  ; 16

(+ (* (call/cc
       (lambda (k)
         (set! retry k) 2)) 3) 10)  ;16

(retry 3) ;19
(retry 2) ;16 
(retry 1) ;13

