#lang plai
; BFAE - L19, L20

; define BFAE type
(define-type BFAE
  [num (n number?)]
  [add (lhs BFAE?) (rhs BFAE?)]
  [sub (lhs BFAE?) (rhs BFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BFAE?)]
  [newbox (v BFAE?)]
  [setbox (bn BFAE?) (v BFAE?)]
  [openbox (v BFAE?)]
  [seqn (ex1 BFAE?) (ex2 BFAE?)]
  [app (ftn BFAE?) (arg BFAE?)])

; parse: sexp -> BFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'newbox v) (newbox (parse v))]
    [(list 'setbox i v) (setbox (parse i) (parse v))]
    [(list 'openbox ex1 ex2) (seqn (parse ex1) (parse ex2))]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    ))

; DefrdSub 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)]) 

; define BFAE-value type
(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
  [boxV (address integer?)])

; Store
(define-type Store
  [mtSto]
  [aSto (address integer?) (value BFAE-Value?)
        (rest Store?)])

; Value*Store
(define-type Value*Store
  [v*s (value BFAE-Value?) (store Store?)])

; num-op: (number number ->number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))) 

(define num+ (num-op +))
(define num- (num-op -))

; lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i adr saved) (if (symbol=? i name)
                          adr
                          (lookup name saved))] ))

; store-lookup address Storee -> BFAE-Value
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto() (error 'store-lookup "No value at address")]
    [aSto (location value rest-store)
          (if (= location address)
              value
              (store-lookup address rest-store))]))

; malloc: Store -> Integer
; purpose: to allocate memory for a new box
(define (malloc st)
  (+ 1 (max-address st)))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
  [mtSto () 0]
  [aSto (n v st)
        (max n (max-address st))]))

; interp: BFAE DefrdSub Store -> Value*Store
(define (interp expr ds st)
  [num (n) (v*s (numV n) st)]
  [id (s) (v*s (store-lookup (lookup s ds) st) st)]
  [fun (p b) (v*s (closureV p b ds) st)]
  [add (l r) (type-case Value*Store (interp l ds st)
               [v*s (l-value l-store)
                    (type-case Value*Store (interp r ds l-store)
                      [v*s (r-value r-store)
                           (v*s (num+ l-value r-value)
                                r-store)])])]
  [sub (l r) (type-case Value*Store (interp l ds st)
               [v*s (l-value l-store)
                    (type-case Value*Store (interp r ds l-store)
                      [v*s (r-value r-store)
                           (v*s (num- l-value r-value)
                                r-store)])])]
  [app (f a) (type-case Value*Store (interp f ds st)
               [v*s (f-value f-store)
                    (type-case Value*Store (interp a ds f-store)
                      [v*s (a-value a-store)
                           (local ([define new-address (malloc a-store)])
                             (interp (closureV-body f-value)
                                     (aSub (closureV-param f-value)
                                           new-address
                                           (closureV-ds f-value))
                                     (aSto new-address
                                           a-value
                                           a-store)))])])]
  [newbox (val)
          (type-case Value*Store (interp val ds st)
          [v*s (vl st1)
               (local [(define a (malloc stt1))]
                 (v*s (boxV a)
                      (aSto a vl st1)))])]  
  [openbox (bx-expr) (type-case Value*Store (interp bx-expr ds st)
                       [v*s (bx-val st1)
                            (v*s (store-lookup (boxV-address bx-val)
                                               st1)
                                 st1)])]
  [setbox (bx-expr val-expr)
          (type-case Value*Store (interp bx-expr ds st)
            [v*s (bx-val st2)
                 (type-case Value*Store (interp val-expr ds st2)
                   [v*s (val st3)
                        (v*s val
                             (aSto (boxV-address bx-val)
                                   val
                                   st3))])])]
  [seqn (a b) (type-case Value*Store (interp a ds st)
                [v*s (a-value a-store)
                     (interp b ds a-store)])]
  )



