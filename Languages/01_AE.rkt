#lang plai
; Lecture 04
; AE
; implement for POSTFIX!! ( ex (1 2 +) )
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])
;(define ae1 (add (sub (num 3) (num 4)) (num 7)))
;(add? ae1)
;(add-lhs ae1)


;parse: sexp->AE
;to convert s-expressions into AEs in abstract syntax
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and (= 3 (length sexp)) (eq? (third sexp) '+)) (add (parse (first sexp)) (parse (second sexp)))]
    [(and (= 3 (length sexp)) (eq? (third sexp) '-)) (sub (parse (first sexp)) (parse (second sexp)))]
    [else (error 'parser "bad syntax: ~a" sexp)] ))
(test (parse '(1 2 +)) (add (num 1) (num 2)))

;interp: AE->number
(define (interp an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]))

(interp (parse '(2 (9 2 -) +))) ;9
(interp (parse '(7 (3 4 -) +))) ;6
