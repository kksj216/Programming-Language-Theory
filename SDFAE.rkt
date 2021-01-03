#lang plai

; SDFAE
(define-type SDFAE
    [num     (n number?)]
    [add     (lhs SDFAE?) (rhs SDFAE?)]
    [sub     (lhs SDFAE?) (rhs SDFAE?)]
    [id        (name symbol?)]
    [fun     (sd symbol?) (param symbol?) (body SDFAE?)]
    [app     (ftn SDFAE?) (arg SDFAE?)])

; parse
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun 's i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'd 'fun (list p) b) (fun 'd p (parse b))]
    [(list 's 'fun (list p) b) (fun 's p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    ))

(parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(app (fun 's 'x (app (fun 's 'f (app (fun 's 'x (app (id 'f) (num 4))) (num 5))) (fun 'd 'y (add (id 'x) (id 'y))))) (num 3))
(parse '{with {x 3} {with {f {s fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(app (fun 's 'x (app (fun 's 'f (app (fun 's 'x (app (id 'f) (num 4))) (num 5))) (fun 's 'y (add (id 'x) (id 'y))))) (num 3))

; just 'with -> static 
; function definition with "d" -> dynamic

; with i v e
; (app (fun i (parse e)) (parse v)

; DefrdSub 
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value SDFAE-Value?) (saved DefrdSub?)])  ; value를 not number?, but FAE-Value?

; FAE-Value
(define-type SDFAE-Value
  [numV (n number?)]
  [closureV (sd symbol?) (param symbol?) (body SDFAE?) (ds DefrdSub?)])

; num-op
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))] ))

; interp
(define (interp sdfae ds)
  (type-case SDFAE sdfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (sd p b) (cond [(symbol=? sd 's) (closureV sd p b ds)]
                        [(symbol=? sd 'd) (closureV sd p b ds)] )]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (interp a ds))]
                 [cond [(symbol=? (closureV-sd f-val) 's)
                        (interp (closureV-body f-val) 
                                (aSub (closureV-param f-val)
                                      a-val
                                      (closureV-ds f-val)))]
                       [(symbol=? (closureV-sd f-val) 'd)
                        (interp (closureV-body f-val) 
                                (aSub (closureV-param f-val)
                                      a-val
                                      ds))] ])] ))

(test (interp (parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub)) (numV 9))
(test (interp (parse '{with {x 3} {with {f {s fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub)) (numV 7))
(interp (parse '{with {y 10} {s fun {x} {+ y x}}}) (mtSub))
(interp (parse '{with {y 10} {d fun {x} {+ y x}}}) (mtSub))
(test (interp (parse '{with {y 10} (with {g {s fun {x} {+ y x}}} {with {y 1} {g 5}})}) (mtSub)) (numV 15))
(test (interp (parse '{with {y 10} {with {g {d fun {x} {+ y x}}} {with {y 1} {g 5}}}}) (mtSub)) (numV 6))

;(interp (parse '{with {z (s fun {x} (+ x y))}}) (mtSub))
;(interp (parse '{with {z (d fun {x} (+ x y))}} {with {y 1} {z 4}}) (mtSub))

;(interp (parse '{with {y 4} {}}))

;(parse '{with {x 3} {with {f {s fun {y} {+ x y}}} {with {x 5} {f 4}}}})
;(interp (app (fun 's 'x (app (fun 's 'f (app (fun 's 'x (app (id 'f) (num 4))) (num 5))) (fun 's 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub))

(interp (parse '(with (y 3) (with {z {s fun {x} {+ x y}}} (with (y 10) (z 2))))) (mtSub))
(interp (parse '(with (y 3) (with {z {d fun {x} {+ x y}}} (with (y 10) (z 2))))) (mtSub))