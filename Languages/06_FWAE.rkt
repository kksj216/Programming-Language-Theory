#lang plai
; FWAE

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?)(named-expr FWAE?)(body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (ftn FWAE?) (arg FWAE?)])

; parse : sexp -> FWAE
(define (parse sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse l) (parse r))]
    [(list '- l r)             (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?)               (id sexp)]
    [(list 'fun (list p) b)    (fun p (parse b))]
    [(list f a)                (app (parse f) (parse a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;num-op: (number number ->number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y))))) ;what is num-n meaning? n is field of num variant (accessor)

(define num+ (num-op +))
(define num- (num-op -))

;(num+ (num 3) (num 25))
;(num- (num 3) (num 25))

; interp: FWAE -> FWAE
(define (interp fwae)
   (type-case FWAE fwae
     [num (n) fwae]
     [add  (l r)    (num+  (interp l) (interp r))]
     [sub  (l r)    (num-  (interp l) (interp r))]
     [with (i v e)  (interp (subst e i (interp v)))]
     [id  (s)       (error 'interp "free identifier")]
     [fun (p b)     fwae]
     [app (f a)     (local [(define ftn (interp f))]
                      (interp (subst (fun-body ftn) (fun-param ftn) (interp a))))]))

; subst
 (define (subst exp idtf val)
   (type-case FWAE exp
     [num (n) exp]
     [add (l r)     (add (subst l idtf val) (subst r idtf val))]
     [sub (l r)     (sub (subst l idtf val) (subst r idtf val))]
     [with (i v e)  (with i (subst v idtf val) (if (symbol=? i idtf) e (subst e idtf val)))]
     [id (s)        (if (symbol=? s idtf) val exp)]
     [app (f a)     (app (subst f idtf val) (subst a idtf val))]
     [fun (id body) (if (equal? idtf id) exp (fun id (subst body idtf val)))]))

(test (parse '{{fun {x} {+ x 1}} 10}) (app (fun 'x (add (id 'x) (num 1))) (num 10)))
(test (interp (parse '{{fun {x} {+ x 1}} 10}) ) (num 11))

(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) (num 10))
(test (interp (with 'x (num 5) (add (num 1) (with 'y (id 'x) (id 'y))))) (num 6))
(test (interp (parse '{fun {a} {+ a a}})) (fun 'a (add (id 'a) (id 'a))))
;(test (interp (parse '{with {x 1} {fn {with {y 10} {+ y x }}}}) (list (parse-fd '{de ffun {fn a} {+ a a}}))) 22);from F1WE
(test (interp (parse '{with {fn {fun {a} {+ a a}}} {with {x 1} {fn {with {y 10} {+ y x}}}}})) (num 22));fn is identifier 
