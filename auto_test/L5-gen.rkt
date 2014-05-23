#lang plai

(define (gen-random-L5 depth)
  (random-L5 depth '()))

; current issue.  How do I stop procedures from being added to numbers?
; need a specific type for L5-vals? numbers and procedures? That's a pain
; Could perhaps I just have two functions?  One that makes number expers
; and other that makes lambda exprs
; well, I basically already have that
; I just need to replace all the places where ONLY number can go with
; (rand-num-exr depth bindings..)
; the other places (bodys of lets) I can put anything
; and in function application, I can only put lambdas


(define arg-vars '(arg1 arg2 arg3 arg4 arg4))
(define var-list '(v1 v2 v3 v4 v5 v6 v7))
(define proc-vars '(f1 f2 f3 f4 f5 f6 f7))
(define array-vars '(a1 a2 a3 a4 a5 a6 a7))

(define-type environment
  [env (nvars (listof symbol?))
       (procs (listof symbol?))
       (arrays (listof symbol?))])

(define/contract (random-L5 depth vars)
  
  (define (random-lambda d bindings)
    (define random-arglist
      (λ (len)
        (if (= len 0)
            '()
            (cons (list-ref arg-vars len)
                  (random-arglist (- len 1))))))
    (let [(lambda-args (random-arglist (random 4)))]
    `(lambda ,lambda-args ,(random-L5 d lambda-args))))
  
  (let ([n (if (= depth 0)
               0 (random 6))]
        [d (- depth 1)])
    (case n
      [(0) (if (= depth 0)
               `(print ,(rand-elm-from (cons (random 20)
                                             vars)))
               `(print ,(random 20)))]
      [(1) `(,(rand-biop) ,(random-L5 d vars)
                          ,(random-L5 d vars))]
      [(2) `(begin ,(random-L5 d vars)
                   ,(random-L5 d vars))]
      [(3) (let ([var-name (rand-varname)])
             `(let ([,var-name ,(random-L5 d vars)])
                ,(random-L5 d (cons var-name vars))))]
      [(4) (random-lambda d vars)]
      [else (random-L5 depth vars)])))

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
 (gen-random-L5 5))






  
  
  
