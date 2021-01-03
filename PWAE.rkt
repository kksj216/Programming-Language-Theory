#lang plai
; HW2
; #1
;  (a) Define type PWAE

(define-type PWAE
  [num (n number?)]
  [op (opr symbol?)]
  [id (i symbol?)]
  [keyword (key symbol?)] 
  [postfix (lhs PWAE?) (rhs PWAE?) (op PWAE?)]
  [substitute (idtf symbol?) (named-expr PWAE?) (body PWAE?) (keyword PWAE?)])

;  (b) Implement Parser for PWAE

(define (parse pexp)
  (match pexp
    [(? number?) (num pexp)]
    [(? symbol?) (cond [(symbol=? pexp 'with) (keyword 'with)]
                       [(symbol=? pexp '+) (op 'add)]
                       [(symbol=? pexp '-) (op 'sub)]
                       [else (id pexp) ])]
    [(list l r '+) (postfix (parse l) (parse r) (parse '+))] 
    [(list l r '-) (postfix (parse l) (parse r) (parse '-))]
    [(list (list i v) e k) (substitute i (parse v) (parse e) (parse k))]
    [else (error 'parse "bad syntax: ~a" pexp)]))
                     
(test (parse '{{3 4 -} 7 +}) (postfix (postfix (num 3) (num 4) (op 'sub)) (num 7) (op 'add)))
(test (parse '{{x 5} {x x +} with}) (substitute 'x (num 5) (postfix (id 'x) (id 'x) (op 'add)) (keyword 'with)))
(test (parse '{{x {5 4 +}} {x x +} with}) (substitute 'x (postfix (num 5) (num 4) (op 'add)) (postfix (id 'x) (id 'x) (op 'add)) (keyword 'with)))


; [contract] subst
(define (subst pwae idtf val)
  (type-case PWAE pwae
    [num (n) pwae]
    [postfix (l r op) (postfix (subst l idtf val) (subst r idtf val) (subst op idtf val))]
    [op (o) 
         (cond
              [(symbol=? o 'add) (op 'add)]
              [(symbol=? o 'sub) (op 'sub)] )]
    [substitute (i v e k) (substitute i (subst v idtf val) (subst e idtf val) k)] 
    [keyword (k) (keyword k)]
    [id (s) (if (symbol=? s idtf) val pwae)]))
 
(test (subst (substitute 's (num 4) (postfix (id 's) (id 's) (op 'add)) (keyword 'with)) 's (num 4)) (substitute 's (num 4) (postfix (num 4) (num 4) (op 'add)) (keyword 'with))) 
;(test (subst (postfix (postfix (num 3) (num 4) (op 'sub)) (num 7) (op 'add))) (postfix (postfix (num 3) (num 4) (op 'sub)) (num 7) (op 'add)))
(test (subst (substitute 'x (num 5) (postfix (id 'x) (id 'x) (op 'add)) (keyword 'with)) 'x (num 5)) (substitute 'x (num 5) (postfix (num 5) (num 5) (op 'add)) (keyword 'with)))
;(subst (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) 'x (num 3))
;(subst (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)))
; ******* substitution in subtitution => no....

;[contract] make-subs: PWAE -> PWAE with substituted  ;take a pwae expression and return substitute expression  
(define (make-subs pwae)
  (type-case PWAE pwae
    [num (n) pwae]
    [substitute (i v e k)
                (match e
                  [(substitute a b c d) (subst (make-subs e) i (make-subs v))]
                  [else (subst pwae i (make-subs v))] )
                ;(match v
                 ; [(substitute a b c d) (subst (make-subs v) i v)]
                  ;[else (subst pwae i (make-subs v))]
                  ;)
                   ]    ;(subst e i (make-subs v))      ;;original : (subst pwae i (make-subs v))
    [postfix (l r op) (postfix (make-subs l) (make-subs r) op)]
    [id (i) pwae]
;        (cond  [(i symbol?) (list (append i))] )] ;pwae] ; (append (list pwae))]
    [keyword (k) (keyword k)]
    [op (o)
         (cond
              [(symbol=? o 'add) (op 'add)]
              [(symbol=? o 'sub) (op 'sub)] )]
  ))

(make-subs (substitute 'x (num 3) (postfix (id 'x) (postfix (num 3) (id 'x) (op 'sub)) (op 'add)) (keyword 'with)))
(+ 0 0)
(make-subs (substitute 'x (num 3) (postfix (id 'a) (postfix (num 4) (id 'x) (op 'add)) (op 'sub)) (keyword 'with)))
(+ 0 1)
(make-subs (substitute 'x (num 3) (postfix (id 'b) (postfix (id 'a) (id 'x) (op 'sub)) (op 'sub)) (keyword 'with)))
(+ 0 2)
(make-subs (substitute 'x (num 3) (postfix (id 'a) (postfix (id 'b) (postfix (id 'x) (id 'b) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with)))
(+ 0 3)
(make-subs (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)))
(+ 0 4)
(make-subs (substitute 'x (id 't) (postfix (id 'x) (substitute 'y (id 'y) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)))
(+ 0 5)
(make-subs (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)))
(+ 0 6)
(make-subs (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'a) (id 'a) (keyword 'with)) (op 'add)))
(+ 0 7)
(make-subs (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add)))
(+ 0 8)
(make-subs (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add)))
(+ 0 9)

;(define (ap-list i)
 ; (append (list i)))

(define (free-ids pwae)
  (define pl (make-subs pwae))
  (type-case PWAE pl
    [num (n) empty]
    [postfix (l r op) (sort (remove-duplicates (append (free-ids l) (free-ids r))) symbol<?)]
    [op (o) empty]
    [substitute (i v e k)  (sort (remove-duplicates (append (free-ids v) (free-ids e))) symbol<?)]
                 
    [keyword (k) empty]
    [id (s) (list s)]  
    ))

;(free-list (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add)))

(test (free-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (num 3) (id 'x) (op 'sub)) (op 'add)) (keyword 'with))) '())
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (num 4) (id 'x) (op 'add)) (op 'sub)) (keyword 'with))) '(a))
(test (free-ids (substitute 'x (num 3) (postfix (id 'b) (postfix (id 'a) (id 'x) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (id 'b) (postfix (id 'x) (id 'b) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b y))
(test (free-ids (substitute 'x (id 't) (postfix (id 'x) (substitute 'y (id 'y) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b t y))
(test (free-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with))) '(x y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'a) (id 'a) (keyword 'with)) (op 'add))) '(a b c y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(b c d y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(b c d y z))



; remove duplicate items in list

  

