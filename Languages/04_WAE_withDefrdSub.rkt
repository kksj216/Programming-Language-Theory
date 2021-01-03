#lang plai
; WAE with defarred substitution

; WAE
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

; DefrdSub 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value number?) (saved DefrdSub?)])

(aSub 'x 1 (aSub 'y 4 (aSub 'x 1 (mtSub))))

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

; [contract] interp: WAE DefrdSub -> number
(define (interp wae ds)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l ds) (interp r ds))]
    [sub (l r) (- (interp l ds) (interp r ds))]
    [with (i v e) (interp e (aSub i (interp v ds) ds))]
    [id (s) (lookup s ds)] ))

; [contract] lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))] ))

(test (lookup 'x (aSub 'x 1 (mtSub))) 1)
(test (lookup 'y (aSub 'x 1 (aSub 'y 4 (mtSub)))) 4)
