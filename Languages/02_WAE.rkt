#lang plai
; Lecture 06 subtitution
; WAE (AE that support identifier)

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

; [contract] parse: sexp->WAE
; [purpose] to convert s-expression into WAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax: ~a" sexp)]))

(test (parse '(+ (- 3 4) 7)) (add (sub (num 3) (num 4)) (num 7)))    
(test (parse '(with (x 5) (+ 8 2))) (with 'x (num 5) (add (num 8) (num 2))))
(test (parse '(with (x 5) (+ x x))) (with 'x (num 5) (add (id 'x) (id 'x))))
(test (parse '(with (x (- 5 2)) (+ x x))) (with 'x (sub (num 5) (num 2)) (add (id 'x) (id 'x))))


; [contract] subst: WAE symbol number -> WAE
; [purpose] to substitute second argument with third argument in first argument,
; as per the rules of substitution; the resulting expression contains no free instances of the second argument
(define (subst wae idft val)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l idft val) (subst r idft val))]
    [sub (l r) (sub (subst l idft val) (subst r idft val))]
    [with (i v e) (with i (subst v idft val) (if (symbol=? i idft) e (subst e idft val)))]
    [id (s) (if (symbol=? s idft) (num val) wae)] ))

(test (subst (num 5) 'x 10) (num 5))
(test (subst (add (num 1) (id 'x)) 'x 10) (add (num 1) (num 10)))
(test (subst (id 'x) 'x 10) (num 10))
(test (subst (id 'y) 'x 10) (id 'y))

; [contract] interp: WAE->number
(define (interp wae)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free identifier")] ))

(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) 10)
(test (interp (with 'x (num 5) (add (num 1) (with 'y (id 'x) (id 'y))))) 6)
(test (interp (parse 'p)) "interp: free identifier")


