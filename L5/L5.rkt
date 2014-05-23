#lang racket
(require rackunit racket/set)

;(struct env (list symbol?) #:transparent)

(define (compile-e e)
  (define top-level-funcs empty)
  (define (create-top-level-function lam free-vars) 'stub)
  (define label-count 0)
  (define (fresh-label)
    (set! label-count (add1 label-count))
    (string->symbol
     (string-append ":f" 
                    (number->string label-count))))
  
  (define/contract (compile-e/env e env)
    (-> (or/c symbol? number? (listof (or/c symbol? number? list?)))
        (listof symbol?)
        list?)
    (match e
      [`(lambda (,args ...) ,body-e) (let [(free-vars (find-frees body-e args '()))]
                                       (begin (create-top-level-function e free-vars)
                                            `(make-closure ,(fresh-label)
                                                           (new-tuple ,free-vars))))] 
      [`(let ([,x ,e]) ,body-e) `(let ([,x ,(compile-e/env e env)])
                                 ,(compile-e/env body-e
                                                 (cons x env)))]
      [`(letrec ([,x ,e]) ,body) 'stub]
      [`(new-tuple ,e ...) `(new-tuple ,@(map (λ (e) (compile-e/env e env)) e))]
      [`(if ,p ,then ,else) `(if ,(compile-e/env p env)
                                 ,(compile-e/env then env)
                                 ,(compile-e/env else env))]  
      [`(,prim ,e1 ,e2) `(,prim ,(compile-e/env e1 env)
                                ,(compile-e/env e2 env))]
      [`(,f ,a ...) 'stub]
      [(? number?) e]
      [(? symbol?) e]))
    (compile-e/env e '())
  )


(define (find-frees e bounds frees)
  (match e
    [`(lambda (,args ...) ,body-e) (find-frees body-e (set-union args bounds) frees)] 
    [`(let ([,x ,e]) ,body-e) (set-union (find-frees e bounds frees)
                                         (find-frees body-e (cons x bounds) frees))]
    [`(letrec ([,x ,e]) ,body-e) (set-union (find-frees e bounds frees)
                                          (find-frees body-e (cons x bounds) frees))]
    [`(new-tuple ,es ...) (foldr set-union '() (map (λ (e) (find-frees e bounds frees)) es))]
    [`(if ,p ,then ,else) (foldr set-union '() (map (λ (e) (find-frees e bounds frees)))
                                 (list p then else))]  
    [`(,prim ,e1 ,e2) (set-union (find-frees e1 bounds frees)
                                 (find-frees e2 bounds frees))]
    [`(,f ,a ...) (foldr set-union '() (map (λ (e) (find-frees e bounds frees))
                                            (cons f a)))]
    [(? number?) frees]
    [(? symbol?) (if (set-member? bounds e)
                     frees
                     (cons e frees))]))


(check-equal? (find-frees '(+ a b) '() '())
              '(b a))
(check-equal? (find-frees '(+ a b) '(a) '())
              '(b))
(check-equal? (find-frees '(new-tuple a 1 2 b 4) '() '())
              '(b a))
(check-equal? (find-frees '(let ([x 5]) (+ x y)) '() '())
              '(y))
(check-equal? (find-frees '(lambda (a b) (let ([x 5])
                             (+ a (+ b (+ c 5))))) '() '())
              '(c))
(check-equal? (find-frees '(lambda (a b c)
                             (let ([x (+ b c)])
                               (new-tuple a b z y e c t)))
                          '() '())
              '(z e t y))
                          
  
 
  
  
(check-equal? (compile-e `(+ 2 1))
              '(+ 2 1))
(check-equal? (compile-e `(new-array a 5))
              '(new-array a 5))
(check-equal? (compile-e `(begin (+ 4 5)
                                 (new-array a 6)))
              '(begin (+ 4 5)
                      (new-array a 6)))
(check-equal? (compile-e `(new-tuple 1 2 3 4 5 (+ 4 4)))
              `(new-tuple 1 2 3 4 5 (+ 4 4)))
(check-equal? (compile-e `(if (= 1 1) (new-tuple a 5)
                              (new-tuple a 6)))
              `(if (= 1 1) (new-tuple a 5)
                   (new-tuple a 6)))

(check-equal? (compile-e `(let ([x 10]) 11))
              `(let ([x 10]) 11))
(check-equal? (compile-e `(let ([x (+ 1 1)]) (+ x 5)))
              `(let ([x (+ 1 1)]) (+ x 5)))
#;
(let ([x 5])
  (let ([y (+ x 6)])
    (let ([x 7])
      (lambda (z) z))))

;(new-tuple y x)
 
;(lambda (x y) (let ([z 10]) (+ x y z)))


    

    



#|
(let ([x 1])
    (let ([f (lambda (y) (+ x y))])
      (f 1)))

((let ([x 1])
    (let ([f (make-closure :f (new-tuple x))])
      ((closure-proc f) (closure-vars f) 1)))
  (:f (vars y) (+ (area vars 0) y)))
|#