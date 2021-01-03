#lang plai
; RCFAE
; L16

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (ftn RCFAE?) (arg RCFAE?)]
  [if0 (test-expr RCFAE?) (then-expr RCFAE?) (else-expr RCFAE?)]
  [rec (name symbol?) (named-expr RCFAE?) (fst-call RCFAE?)])

; RCFAE-Value
(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (ds DefrdSub?)]
  [exprV (expr RCFAE?) (ds DefrdSub?) (value (box/c (or/c false RCFAE-Value?)))])

; DefrdSub 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value RCFAE-Value?) (saved DefrdSub?)]  ; value를 not number?, but FAE-Value?
  [aRecSub (name symbol?) (value-box (box/c RCFAE-Value?)) (ds DefrdSub?)])

; lookup: symbol DefrdSub -> RCFAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variavle")]
    [aSub (sub-name val rest-ds)
          (if (symbol=? sub-name name)
              val
              (lookup name rest-ds))]
    [aRecSub (sub-name val-box rest-ds)
             (if (symbol=? sub-name name)
                 (unbox val-box)
                 (lookup name rest-ds))]))

; strict: RCFAE-Value -> RCFAE-Value
(define (strict v)
  (type-case RCFAE-Value v
    [exprV (expr ds v-box)
           (if (not (unbox v-box))
               (local [(define v (strict (interp expr ds)))]
                 (begin (set-box! v-box v)
                  v))
               (unbox))]
    [else v]))  

; parse : sexp -> RCFAE
(define (parse sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse l) (parse r))]
    [(list '- l r)             (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?)               (id sexp)]
    [(list 'fun (list p) b)    (fun p (parse b))]
    [(list f a)                (app (parse f) (parse a))]
    [(list 'if0 ex1 ex2 ex3) (if0 (parse ex1) (parse ex2) (parse ex3))] 
    [(list 'rec (list n ex) ft) (rec n (parse ex) (parse ft))] 
    [else                (error 'parse "bad syntax: ~a" sexp)])) 

; num-op: (number number ->number) -> (RCFAE RCFAE -> RCFAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n (strict x)) (numV-n (strict y)))))) ;what is num-n meaning? n is field of num variant (accessor)

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))

; interp: RCWAE -> RCWAE
(define (interp rcfae ds)
   (type-case RCFAE rcfae
     [num (n) (numV n)]
     [add (l r) (num+  (interp l ds) (interp r ds))]
     [sub (l r) (num-  (interp l ds) (interp r ds))]
     [id (s) (lookup s ds)]
     [fun (param body-expr) (closureV param body-expr ds)]
     [app (f a)     (local [(define ftn (interp f ds))]
                      (interp (closureV-body ftn)
                              (aSub (closureV-param ftn)
                                    (interp a ds)
                                    (closureV-ds ftn))))]
     [if0 (test-expr then-expr else-expr) (if (numzero? (interp test-expr ds)) 
                                              (interp then-expr ds)
                                              (interp else-expr ds))]
     [rec (bound-id named-expr fst-call)
           (local [(define value-holder (box (numV 200)))
                   (define new-ds (aRecSub bound-id
                                           value-holder
                                           ds))]
             (begin (set-box! value-holder (interp named-expr new-ds))
                    (interp fst-call new-ds)))]))
     ;[rec (bound-id named-expr fst-call)
     ;        (interp fst-call
     ;                (aSub bound-id
     ;                      (interp name-expr ds)
     ;                      ds))]))

; numzero? RCFAE-Value -> boolean
(define (numzero? n)
  (zero? (numV-n n)))


(define (run expr ds)
  (interp (parse expr) ds))

;(run '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}}
;        {count 8}} (mtSub))
(parse '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}})
; -->
(rec 'count (fun 'n (if0 (id 'n) (num 0) (add (num 1) (app (id 'count) (sub (id 'n) (num 1)))))) (app (id 'count) (num 8)))

(parse '{fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}})
(parse '{if0 n 0 {+ 1 {count {- n 1}}}})
(fun 'n (if0 (id 'n) (num 0) (add (num 1) (app (id 'count) (sub (id 'n) (num 1))))))
(if0 (id 'n) (num 0) (add (num 1) (app (id 'count) (sub (id 'n) (num 1)))))

(run '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}} (mtSub))

;(rec (count (fun (n) (if0 n 0 (+ 1 (count (- n 1)))))) (count 8))


