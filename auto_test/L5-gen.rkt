#lang plai



; current issue.  How do I stop procedures from being added to numbers?
; need a specific type for L5-vals? numbers and procedures? That's a pain
; Could perhaps I just have two functions?  One that makes number expers
; and other that makes lambda exprs
; well, I basically already have that
; I just need to replace all the places where ONLY number can go with
; (rand-num-exr depth bindings..)
; the other places (bodys of lets) I can put anything
; and in function application, I can only put lambdas

#|
e ::= (lambda (x ...) e)
    | x
    | (let ([x e]) e)
    | (letrec ([x e]) e)
    | (if e e e)
    | (new-tuple e ...)
    | (begin e e)
    | (e e ...) ;; application expression
    | prim
    | num

| (lambda (x ...) any-e)
| x
| (let ([x any-e]) any-e)
| (letrec ([x any-e]) any-e)
| (if num-e any-e any-e)
| (new-tuple any-e ...)
| (begin any-e any-e)
| (proc-e any-e ...)
| (biop num-e)
| (pred any-e)
| (print num-e/arr-e)
| (new-array num-e any-e)
| (aref arr-e num-e)
| (aset arr-e num-e any-e)
| (alen arr-e)

biop ::= + | - | * | < | <= | =
pred ::= number? | a?


|#
(define (gen-random-L5 depth)
  `(print ,(random-L5 depth '())))

(define param-vars '(par1 par2 par3 par4 par4))
(define var-list '(v1 v2 v3 v4 v5 v6 v7))
(define proc-vars '(f1 f2 f3 f4 f5 f6 f7))
(define array-vars '(a1 a2 a3 a4 a5 a6 a7))

(define-type environment
  [env (nvars (listof symbol?))
       (procs (listof symbol?))
       (arrays (listof symbol?))])

(define (random-L5 depth vars) 
  (let ([n (if (= depth 0)
               0
               (+ 1 (random 6)))]
        [d (- depth 1)])
    (case n
      [(0)  (if (empty? vars)
                (random 20)
                (rand-elm-from vars))]
      [(1) `(,(rand-biop) ,(random-L5 d vars)
                          ,(random-L5 d vars))]
      [(2) `(begin ,(random-L5 d vars)
                   ,(random-L5 d vars))]
      [(3) (let ([var-name (rand-varname)])
             `(let ([,var-name ,(random-L5 d vars)])
                ,(random-L5 d (cons var-name vars))))]
      [(4) (let* ([arity (random 5)]
                  [params (take param-vars arity)]
                  [lam (random-lambda params d vars)]
                  [args (list-of-n-L5s arity d vars)])
             `(,lam ,@args))]
      [(5) `(,(rand-elm-from '(number? a?)) (random-L5 d vars))]
      [else (random-L5 depth vars)])))

(define (random-lambda params depth binds)
  `(lambda ,params ,(random-L5 depth (append params binds))))

(define (list-of-n-L5s n depth vars)
  (for/list [(i (in-range n))]
    (random-L5 depth vars)))


(define (rand-varname)
    (rand-elm-from var-list))

(define (rand-elm-from lst)
  (let ([n (random (length lst))])
    (list-ref lst n)))

(define (rand-biop)
  (rand-elm-from '(+
                   -
                   *
                   <
                   <=
                   =
                   )))

(pretty-write
 (gen-random-L5 4))






  
  
  
