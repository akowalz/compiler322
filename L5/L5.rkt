#lang racket
(require rackunit racket/set)

; === Keyword/primitive lists ===
(define keywords
  '(begin
     number?
     a?
     print
     new-array
     aref
     aset
     alen
     +
     -
     *
     <
     <=
     =))

(define two-arity-prims
  '(+
    -
    *
    <
    <=
    =
    aref
    new-array))

(define single-arity-prims
  '(a? number? print alen?))



;; Main Compilation Function  ============================================
(define (compile-e e)
  
  ;; Top level function generation
  (define top-level-funcs empty) 
  (define (create-top-level-function body-e free-vars args fn-name)
    (set! top-level-funcs
          (cons
           (if (> (length args) 2)
               `(,fn-name (vars-tuple args-tuple)
                          ,(unpack-frees free-vars 0
                                         (unpack-args args 0 body-e)))
               `(,fn-name (vars-tuple ,@args)
                          ,(unpack-frees free-vars 0 body-e)))
           top-level-funcs)))
  
  
  ;; Label and variable generation
  (define label-count 0) 
  (define (fresh-label)
    (set! label-count (add1 label-count))
    (string->symbol
     (string-append ":f" 
                    (number->string label-count))))
  (define var-count 0)
  (define (fresh-varname)
    (set! var-count (add1 var-count))
    (string->symbol
     (string-append "v"
                    (number->string var-count))))
  
  ;; Internal Compile-e function
  (define (compile-e-int e)
    (match e
      [`(lambda (,args ...) ,body-e) (let [(free-vars (find-frees body-e args '()))
                                           (fn-name (fresh-label))]
                                       (begin (create-top-level-function (compile-e-int body-e) free-vars args fn-name)
                                              `(make-closure ,fn-name
                                                             (new-tuple ,@free-vars))))] 
      [`(let ([,x ,e]) ,body-e) `(let ([,x ,(compile-e-int e)])
                                   ,(compile-e-int body-e))]
      [`(letrec ([,x ,e]) ,body) `(let ([,x (new-tuple 0)])
                                    (begin
                                      (aset ,x 0 ,(compile-e-int (replace-var x `(aref ,x 0) e) ))
                                      ,(compile-e-int (replace-var x `(aref ,x 0) body) )))]
      [`(new-tuple ,e ...) `(new-tuple ,@(map (λ (e) (compile-e-int e)) e))]
      [`(if ,p ,then ,else) `(if ,(compile-e-int p)
                                 ,(compile-e-int then)
                                 ,(compile-e-int else))]
      [`(,f ,a ...) 
       ; test for primitives, just call
       (if (set-member? keywords f)
           `(,f ,@(map compile-e-int a))
           ; otherwise, transform to closure calling convention
           (let ([fun (fresh-varname)]
                 [arglist (if (> (length a) 2)
                              `((new-tuple ,@a))
                              a)])
             `(let ([,fun ,(compile-e-int f)])
                ((closure-proc ,fun) (closure-vars ,fun)
                                     ,@(map compile-e-int arglist)))))]
      
      [(? number?) e]
      
      [(? symbol?) ;if value being passed around is a primitive op, find it's matching 
       ;lambda-expr, compile it, and return it.  Otherwise return the symbol
       (if (set-member? keywords e)
           (compile-e-int (prim-lambda e) )
           e)]))
  
  
  ; Calling compile-e, with special case guards
  (if (or (number? e) (symbol? e))
      (list 0)
      (cons (compile-e-int e) top-level-funcs))
  )


; finds free variables in an e and replaces them with new-var
; returns e if the specified var is shadowed
(define (replace-var var new-var e)
  (define (let-replacement x var new-var e body-e let-type)
  (if (equal? x var)
      `(,let-type
        ([,x ,(replace-var var new-var e)])
         ,body-e)
      `(,let-type ([,x ,(replace-var var new-var e)])
         ,(replace-var var new-var body-e))))
  (match e
    [`(lambda (,args ...) ,body-e) (if (set-member? args var)
                                       e
                                       `(lambda ,args ,(replace-var var new-var body-e)))]
    [`(let ([,x ,e1]) ,body-e) (let-replacement x var new-var e1 body-e 'let)]
    [`(letrec ([,x ,e1]) ,body-e) (let-replacement x var new-var e1 body-e 'letrec)]
    [_ (replace-in-list var new-var e)]))

(define (let-replacement x var new-var e body-e rec?)
  (if (equal? x var)
      `(,(if rec?
             'letrec
             'let)
            ([,x ,(replace-var var new-var e)])
         ,body-e)
      `(,(if rec?
             'letrec
             'let) ([,x ,(replace-var var new-var e)])
         ,(replace-var var new-var body-e))))

; simple recursive list-replace function (has known bug....)
(define (replace-in-list var new-var e)
  (cond [(empty? e) '()]
        [(number? e) e]
        [(symbol? (first e)) (if (equal? (first e) var)
                                 (cons new-var (replace-in-list var new-var (rest e)))
                                 (cons (first e) (replace-in-list var new-var (rest e))))]
        [(list? (first e)) (cons (replace-in-list var new-var (first e))
                                 (replace-in-list var new-var (rest e)))]
        [else (cons (first e) (replace-in-list var new-var (rest e)))]))

; finds all free variables in an e, given a set of current bindings
; as a final step, remove all keywords/primitives
(define (find-frees e bounds frees)
  (define (find-all-frees es)
    (foldr set-union '() (map (λ (e) (find-frees e bounds frees)) es)))
  (set-subtract 
   (match e
     [`(lambda (,args ...) ,body-e) (find-frees body-e (set-union args bounds) frees)] 
     [`(let ([,x ,e]) ,body-e) (set-union (find-frees e bounds frees)
                                          (find-frees body-e (cons x bounds) frees))]
     [`(letrec ([,x ,e]) ,body-e) (set-union (find-frees e bounds frees)
                                             (find-frees body-e (cons x bounds) frees))]
     [`(new-tuple ,es ...) (find-all-frees es)]
     [`(if ,p ,then ,else) (find-all-frees (list p then else))]  
     [`(,f ,a ...) (find-all-frees (cons f a))]
     [(? number?) frees]
     [(? symbol?) (if (set-member? bounds e)
                      frees
                      (cons e frees))])
   keywords))

; generate the appropriate lambda expression for a given primitive
(define (prim-lambda prim)
  (cond [(set-member? single-arity-prims prim) `(lambda (q) (,prim q))]
        [(set-member? two-arity-prims prim) `(lambda (q p) (,prim q p))]
        [(symbol=? 'aset prim) `(lambda (q p r) (aset p q r))]))

; unpack tuples of free variables and arguments into nested let expressions
(define (unpack-frees free-vars count body)
  (if (= (length free-vars) 0)
      body
      `(let ((,(first free-vars) (aref vars-tuple ,count)))
         ,(unpack-frees (rest free-vars) (+ 1 count) body))))

(define (unpack-args args count body)
  (if (= (length args) 0)
      body
      `(let ((,(first args) (aref args-tuple ,count)))
         ,(unpack-args (rest args) (+ 1 count) body))))


; command line stuff =========================================================

(if (= (vector-length (current-command-line-arguments)) 1)
    (call-with-input-file
        (vector-ref (current-command-line-arguments) 0)
      (λ (x) (display (compile-e (read x)))))
    (display ""))


;; ____________________________________
;;|__   __|  ____|/ ____|__   __/ ____|
;;;;;| |  | |__  | (___    | | | (___  
;;;;;| |  |  __|  \___ \   | |  \___ \ 
;;;;;| |  | |____ ____) |  | |  ____) |
;;;;;|_|  |______|_____/   |_| |_____/ 

(check-equal? (unpack-frees '(a b c) 0 '(+ 1 2))
              '(let ((a (aref vars-tuple 0)))
                 (let ((b (aref vars-tuple 1)))
                   (let ((c (aref vars-tuple 2)))
                     (+ 1 2)))))

(check-equal? (unpack-frees '(a) 0 (unpack-args '(b) 0 '(+ a b)))
              '(let ((a (aref vars-tuple 0)))
                 (let ((b (aref args-tuple 0)))
                   (+ a b))))

(check-equal? (find-frees '(+ a b) '() '())
              '(b a))
(check-equal? (find-frees '(+ a b) '(a) '())
              '(b))
(check-equal? (find-frees '(new-tuple a 1 2 b 4) '() '())
              '(a b))
(check-equal? (find-frees '(let ([x 5]) (+ x y)) '() '())
              '(y))
(check-equal? (find-frees '(lambda (a b) (let ([x 5])
                                           (+ a (+ b (+ c 5))))) '() '())
              '(c))
(check-equal? (find-frees '(lambda (a b c)
                             (let ([x (+ b c)])
                               (new-tuple a b z y e c t)))
                          '() '())
              '(y t e z))


(check-equal? (compile-e `(lambda (x) (+ x 1)))
              '((make-closure :f1 (new-tuple)) (:f1 (vars-tuple x) (+ x 1))))

(check-equal? (compile-e `(let [(x 1)] (lambda (y) (+ x y))))
              '((let ((x 1)) (make-closure :f1 (new-tuple x)))
                (:f1 (vars-tuple y) (let ((x (aref vars-tuple 0))) (+ x y)))))
(check-equal? (compile-e '(let ([x 1]) ((lambda (y) (+ x y)) 4)))
              `((let ([x 1]) (let ([v1 (make-closure :f1 (new-tuple x))])
                               ((closure-proc v1) (closure-vars v1) 4)))
                (:f1 (vars-tuple y) (let ([x (aref vars-tuple 0)])
                                      (+ x y)))))



(check-equal? (compile-e `(+ 2 1))
              '((+ 2 1)))
(check-equal? (compile-e `(new-array a 5))
              '((new-array a 5)))
(check-equal? (compile-e `(begin (+ 4 5)
                                 (new-array a 6)))
              '((begin (+ 4 5)
                       (new-array a 6))))
(check-equal? (compile-e `(new-tuple 1 2 3 4 5 (+ 4 4)))
              `((new-tuple 1 2 3 4 5 (+ 4 4))))
(check-equal? (compile-e `(if (= 1 1) (new-tuple a 5)
                              (new-tuple a 6)))
              `((if (= 1 1) (new-tuple a 5)
                    (new-tuple a 6))))

(check-equal? (compile-e `(let ([x 10]) 11))
              `((let ([x 10]) 11)))
(check-equal? (compile-e `(let ([x (+ 1 1)]) (+ x 5)))
              `((let ([x (+ 1 1)]) (+ x 5))))
(check-equal? (compile-e `(print 5)) `((print 5)))
(check-equal? (compile-e '(print (+ 1 2) (+ 1 2)))
              '((print (+ 1 2) (+ 1 2))))


(check-equal? (compile-e `(letrec ((y (+ y 1))) (+ y 5)))
              `((let ([y (new-tuple 0)])
                  (begin (aset y 0 (+ (aref y 0) 1))
                         (+ (aref y 0) 5)))))


(check-equal? (compile-e `(let ([func_with_private_var ((lambda ()
                                                          (let ([x 101])
                                                            (lambda () x))))])
                            (print (func_with_private_var))))
              '((let ((func_with_private_var
                       (let ((v1 (make-closure :f1 (new-tuple))))
                         ((closure-proc v1) (closure-vars v1)))))
                  (print
                   (let ((v2 func_with_private_var))
                     ((closure-proc v2) (closure-vars v2)))))
                (:f1 (vars-tuple) (let ((x 101)) (make-closure :f2 (new-tuple x))))
                (:f2 (vars-tuple) (let ((x (aref vars-tuple 0))) x))))

(check-equal? (compile-e '+) '(0))


(check-equal? (compile-e '(let ([fn +]) 
                            (print (fn 17 189))))
              '((let ((fn (make-closure :f1 (new-tuple))))
                  (print
                   (let ((v1 fn)) ((closure-proc v1) (closure-vars v1) 17 189))))
                (:f1 (vars-tuple q p) (+ q p))))




;; Known failing tests (just a selection....)

#;
(print
 (let ((x (new-array 10 11)))
   (begin (((lambda (x) x) aset) x 1 202) (aref x 1))))

#;
(compile-e `(let ((y 1)) (letrec ((f (lambda (x) y))) (print (f 2)))))







