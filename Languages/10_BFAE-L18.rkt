#lang plai
; BFAE - L18

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

; DefrdSub 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)]) 

; define BFAE-value type
(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
  ;[boxV (container (box/c BFAE-Value?))])
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

; interp: BFAE DefrdSub -> BFAE-Value
(define (interp bfae ds)
  (type-case BFAE bfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [newbox (val-expr)
            (boxV (box (interp val-expr ds)))]
    [setbox (box-expr val-expr)
            (set-box! (boxV-container (interp box-expr ds))
                      (interp val-expr ds))]
    [openbox (box-expr)
             (unbox (boxV-container (interp box-expr ds)))]
    [seqn (e1 e2)
          (interp e1 ds)
          (interp e2 ds)]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (interp a ds))] 
                 (interp (closureV-body f-val)
                         (aSub (closureV-param f-val)
                               a-val
                               (closureV-ds f-val))))]))
 

{with {b {newbox 7}}
      {seqn {setbox b 10}
            {openbox b}}}



