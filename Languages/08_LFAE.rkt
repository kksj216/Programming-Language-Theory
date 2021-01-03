#lang plai
; LFAE

(define-type LFAE
  [num (n number?)]
  [add (lhs LFAE?) (rhs LFAE?)]
  [sub (lhs LFAE?) (rhs LFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body LFAE?)]
  [app (ftn LFAE?) (arg LFAE?)])

; DefrdSub 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value LFAE-Value?) (saved DefrdSub?)])
  ;[exprV (expr LFAE?) (ds DefrdSub?)])  ; value를 not number?, but FAE-Value?

; LFAE-Value
(define-type LFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body LFAE?) (ds DefrdSub?)]
  [exprV (expr LFAE?) (ds DefrdSub?) (value (box/c (or/c  false LFAE-Value?)))])

; parse
(define (parse sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse l) (parse r))]
    [(list '- l r)             (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?)               (id sexp)]
    [(list 'fun (list p) b)    (fun p (parse b))]
    [(list f a)                (app (parse f) (parse a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;num-op: (number number ->number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))) ;what is num-n meaning? n is field of num variant (accessor)

(define num+ (num-op +))
(define num- (num-op -))


; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))] ))

; strict: LFAE-Value
(define (strict v)
  (type-case LFAE-Value v
    [exprV (expr ds v-box)
           (if (not (unbox v-box))
               (local [(define v (strict (interp expr ds)))]
                 (begin (set-box! v-box v)
                        v))
               (unbox v-box))]
    [else v]))

; interp: LFAW DefrdSub -> FAW-Value 
(define (interp fae ds)
  (type-case LFAE fae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (name) (lookup name ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local [(define ftn-v (strict (interp f ds)))
                       ;(define arg-v (exprV a ds))]
                       (define a-val (exprV a ds (box #f)))]
                 (interp (closureV-body ftn-v)
                         (aSub (closureV-param ftn-v)
                               ;arg-v
                               a-val
                               (closureV-ds ftn-v))))]))

