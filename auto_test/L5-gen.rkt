#lang plai

(define (gen-random-L5 depth)
  (random-L5 depth '()))

(define arg-vars '(x y z t q))

(define (random-L5 depth vars)
  
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
      [else (random-L5 d vars)])))

(define (rand-varname)
    (rand-elm-from '(var1 var2 var3 var4)))

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






  
  
  
