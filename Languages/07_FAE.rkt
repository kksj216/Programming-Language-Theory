#lang plai
; FAE

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (ftn FAE?) (arg FAE?)])

; DefrdSub 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (saved DefrdSub?)])  ; value를 not number?, but FAE-Value?

; FAE-Value
(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)])

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

; no more with branch,
; (parse {with {x 3} {+ x x}})
;   at FAE, => (app (fun 'x (add (id 'x) (id 'x))) (num 3))

;num-op: (number number ->number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))) ;what is num-n meaning? n is field of num variant (accessor)

(define num+ (num-op +))
(define num- (num-op -))

; interp: FAE -> FAE  (with subst function)
;(define (interp fae)
;  (type-case FAE faee
;    [num (n) fae]
;    [add (l r) (num+ (interp l) (interp r))]
;    [sub (l r) (num- (interp l) (interp r))]
;    [id (s) (error 'interp "free identifier")]
;    [fun (p b) fae]
;    [app (f a) (local [(define ftn (interp f))]
;                 (interp (subst (fun-body ftn)
 ;                               (fun-param ftn)
  ;                              (interp a))))] ))


; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))] ))

; interp: FAE DefrdSub -> FAE (with defrd substitution)
; (define (interp fae ds)
;  (type-case FAE fae
;    [num (n) fae]
;    [add (l r) (num+ (interp l) (interp r))]
;    [sub (l r) (num- (interp l) (interp r))]
;    [id (s) (lookup s ds)]
;    [fun (p b) fae]
;    [app (f a) (local ([define ftn (interp f ds)])
;                      (interp (fun-body ftn) 
;                      (aSub (fun-param ftn)
;                           (interp a ds)
;                            ds)))]))

; In FAE, function can be used as value, so in aSub we have to use ds.

; interp: FAW DefrdSub -> FAW-Value 
(define (interp fae ds)
  (type-case FAE fae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (interp a ds))] 
                 (interp (closureV-body f-val)
                         (aSub (closureV-param f-val)
                               a-val
                               (closureV-ds f-val))))]))
 
;(app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3))

(parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})

(interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))
(interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub))


(parse '{with {z {fun {x} {+ x y}}} {with {y 10} z}})

(parse '{with {fac {fun {n} {+ n {fac {- n 1}}}}} {fac 10}})  ; fac is free identifier
(app (fun 'fac (app (id 'fac) (num 10))) (fun 'n (add (id 'n) (app (id 'fac) (sub (id 'n) (num 1))))))

(interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))