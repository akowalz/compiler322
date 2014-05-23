#lang plai

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
  `(print ,(random-L5 depth (env '() '() '()))))

(define param-vars '(par1 par2 par3 par4 par4))
(define var-list '(v1 v2 v3 v4 v5 v6 v7))
(define proc-vars '(f1 f2 f3 f4 f5 f6 f7))
(define array-vars '(a1 a2 a3 a4 a5 a6 a7))
(define biops '(+ - * < <= =))

(define-type environment
  [env (nvars (listof symbol?))
       (procs (listof symbol?))
       (arrays (listof symbol?))])

(define/contract (random-L5 depth binds)
  (-> exact-nonnegative-integer? environment? list?)
  
  (let ([n (if (= depth 0)
               0
               (+ 1 (random 8)))]
        [d (- depth 1)])
    
    (define (rand-biop-expr)
      `(,(rand-biop) ,(random-L5 d binds)
                     ,(random-L5 d binds)))
    (define (rand-value)
      (if (empty? (env-nvars binds))
          (random 20)
          (rand-elm-from (env-nvars binds))))
    (define (rand-begin-expr)
      `(begin ,(random-L5 d binds)
              ,(random-L5 d binds)))
    (define (rand-let-expr)
      (let* ([var-type (rand-elm-from '(num proc))]
             [var-name (if var-type
                           (rand-elm-from proc-vars)
                           (rand-varname))])
        (case var-type
          ['num `(let ([,var-name ,(random-L5 d binds)])
                   ,(random-L5
                     d 
                     (type-case environment binds
                       (env (vs ps as)
                            (env (cons var-name vs)
                                 ps
                                 as)))))]
          ['proc `(let ([,var-name ,(random-lambda 3 d binds)])
                    ,(random-L5
                      d
                      (type-case environment binds
                        (env (vs ps as)
                             (env vs
                                  (cons var-name ps)
                                  as)))))])))
    (define (rand-app-expr)
      (let* ([arity (if (empty? (env-procs binds))
                        (random 5)
                        3)]
             [lam (if (empty? (env-procs binds))
                      (random-lambda arity d binds)
                      (rand-elm-from (env-procs binds)))]
             [args (list-of-n-L5s arity d binds)])
        `(,lam ,@args)))
    (define (rand-pred-expr)
      `(,(rand-elm-from '(number? a?)) ,(random-L5 d binds)))
    (define (rand-if-expr)
      `(if ,@(list-of-n-L5s 3 d binds)))
    
    (case n
      [(0) (rand-value)]
      [(1) (rand-biop-expr)]
      [(2) (rand-begin-expr)]
      [(3) (rand-let-expr)]
      [(4) (rand-app-expr)]
      [(5) (rand-pred-expr)]
      [(6) (rand-if-expr)]
      [else (random-L5 depth binds)])))


(define (random-lambda arity depth binds)
  (let ([params (take param-vars arity)])
    `(lambda ,params
       ,(random-L5 depth
                   (type-case environment binds
                     (env (vs ps as)
                          (env (append params vs)
                               ps
                               as)))))))

(define (list-of-n-L5s n depth vars)
  (for/list [(i (in-range n))]
    (random-L5 depth vars)))

(define (rand-varname)
    (rand-elm-from var-list))

(define (rand-elm-from lst)
  (let ([n (random (length lst))])
    (list-ref lst n)))

(define (rand-biop)
  (rand-elm-from biops))

(pretty-write
 (gen-random-L5 5))






  
  
  
