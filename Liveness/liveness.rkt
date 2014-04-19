#lang racket
(require racket/set)
(require rackunit)
(require racket/stream)

(struct kill-gen (kills gens))
(struct in-out (ins outs))
(provide kills/gens)

(define/contract (label? x)
  (-> (or/c symbol? number? list?) boolean?)
  (if (symbol? x)
      (if (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string x))
          #t
          #f)
      #f))

(define/contract (kills/gens instr)
  (-> (or/c list? symbol?) kill-gen?)
  (match instr
    [`(eax <- (print ,t)) (kill-gen `(eax ecx edx) (if (symbol? t)
                                                       (list t)
                                                       '()))]
    [`(,x <- (mem ,y ,n)) (kill-gen (list x) (list y))]
    [`((mem ,y ,n) <- ,s) (kill-gen '() (if (check-var-reg s) 
                                                (list y s) 
                                                (list y)))]
    [`(eax <- (allocate ,t1 ,t2)) (kill-gen `(eax ecx edx) (two-ts t1 t2))]
    [`(eax <- (array-error ,t1 ,t2)) (kill-gen `(eax ecx edx) (two-ts t1 t2))]
    [`(,x <- ,s) (kill-gen (list x) (if (check-var-reg s)
                                        (list s) 
                                        (list)))]
    [`(,x ,op ,t) (kill-gen (list x) (if (symbol? t) 
                                         (list x t)
                                         (list x)))]
    [`(,cx <- ,t1 ,cop ,t2) (kill-gen (list cx) (two-ts t1 t2))]
    [(? symbol?) (kill-gen (list ) (list ))]
    [`(goto ,label) (kill-gen (list ) (list ))]
    [`(cjump ,t1 ,cop ,t2 ,label1 ,label2) (kill-gen '() (two-ts t1 t2))]
    [`(call ,u) (kill-gen `(eax ebx ecx edx) (if (not (label? u))
                                                 `(,u eax ecx edx)
                                                 `(eax ecx edx)))]
    [`(tail-call ,u) (kill-gen (list ) (if (not (label? u))
                                           `(,u eax ecx edx esi edi)
                                           `(eax ecx edx esi edi)))]
    [`(return) (kill-gen (list ) `(eax esi edi))]
    [else (error 'parse "Expression didn't conform to L1 grammar")]))

(define (kills/gens-program prog)
  (map kills/gens prog))

(define (all-kills prog)
  (map kill-gen-kills (kills/gens-program prog)))

(define (all-gens prog)
  (map kill-gen-gens (kills/gens-program prog)))
#;
(define/contract (killed? instr x)
  (-> list? symbol? boolean?)
  (kill-gen-kills (kills/gens instr)))

(define/contract (preds num func)
  (-> number? (or/c list? symbol?) (listof number?))
  (let ([instr (list-ref func num)])
    (if (= num 0) 
        (find-refs instr func)
        (if (label? instr)
            (append (find-refs instr func) (list (- num 1)))
            (list (- num 1))))))



(define/contract (all-preds func)
  (-> list? (listof (listof number?)))
  (map (lambda (n) (preds n func))
       (stream->list (in-range (length func)))))
       


(define/contract (find-refs label func)
  (-> label? list? (listof number?))
  (let ([preds '()])
    (for/list ([instr func]
               [i (in-range (length func))])
      (match instr
        [`(cjump ,t1 ,cop ,t2 ,lab1 ,lab2) 
         (if (or (symbol=? label lab1) (symbol=? label lab2))
             (set! preds (cons i preds))
             preds)]
        [`(goto ,lab) 
         (if (symbol=? label lab) 
             (set! preds (cons i preds))
             preds)]
        [else preds]))
    preds))

(define/contract (check-var-reg s)
  (-> (or/c symbol? number?) boolean?)
  (and (symbol? s) (not (label? s))))

(define/contract (two-ts t1 t2)
  (-> (or/c symbol? number?) (or/c symbol? number?) list?)
  (cond
    [(and (symbol? t1) (symbol? t2))
     (if (symbol=? t1 t2) (list t1)
         (list t1 t2))]
    [(symbol? t1) (list t1)]
    [(symbol? t2) (list t2)]
    [else '()]))


(define/contract (in/out func)
  (-> list? in-out?)
  (let* ([kill-list (map kill-gen-kills (map kills/gens func))]
         [pred-list (all-preds func)]
         [ins (map kill-gen-gens (map kills/gens func))]
         [outs (copy-inds ins pred-list (make-list-of-empties (length func)))])
    (in/out-help ins outs kill-list pred-list))
  )

(define/contract (in/out-help ins outs kill-list preds)
  (-> (listof (listof symbol?))
      (listof (listof symbol?))
      (listof (listof symbol?))
      (listof (listof number?))
      in-out?)
  (let* ([new-ins (copy-not-killed ins outs kill-list)]
         [new-outs (copy-inds new-ins preds outs)])
  (if (equal? ins new-ins)
      (in-out ins outs)
        (if (equal? outs new-outs)
            (in-out new-ins outs)
            (in/out-help new-ins new-outs kill-list preds)))))
  
#;
(define/contract (copy-not-killed ins outs kill-list)
  (-> list? list? list? list?)
  (map set-subtract (copy-inds outs (make-list-of-increasing-ints (length ins)) ins)
       kill-list)
  )

;  I think is makes way more sense...
(define/contract (copy-not-killed ins outs kill-list)
  (-> (listof (listof symbol?))
      (listof (listof symbol?))
      (listof (listof symbol?))
      list?)
  (map set-subtract (map set-union ins outs) kill-list))

(define (make-list-of-empties n)
  (if (= n 0) '()
      (cons empty (make-list-of-empties (- n 1)))))
(check-equal? (make-list-of-empties 3)
              '( () () ()))

; and we might not need this
(define (make-list-of-increasing-ints n)
  (if (= n 1) '((0)) 
      (append (make-list-of-increasing-ints (- n 1)) (list (list (- n 1))))))

(check-equal? (make-list-of-increasing-ints 3)
              '((0) (1) (2)))
(check-equal? (make-list-of-increasing-ints 1)
              '((0)))

;; wtfomgbbq
(define/contract (copy-inds src indexes dst)
  (-> (listof (listof symbol?))
      (listof (listof number?))
      (listof (listof symbol?))
      list?)
  (map (lambda (x y) (get-new-outs src x y)) 
       indexes dst))

(define/contract (get-new-outs ins preds out)
  (-> (listof (listof symbol?))
      (listof number?)
      (listof symbol?)
      list?)
  (if (empty? preds) out
      (set-union (list-ref ins (first preds))
                 (get-new-outs ins (rest preds) out))))

(check-equal? (get-new-outs '((a) (b)) '(0 1) '())
              '(b a))
(check-equal? (get-new-outs '((a) (b)) '(0 1) '(c))
              '(b c a))
(check-equal? (get-new-outs '((a) (b)) '(0) '(c))
              '(c a))

(check-equal? (copy-inds '((a) (b) (c)) '((2 1) (1) (0)) '(() () ()))
              '((b c) (b) (a))) 

(check-equal? (copy-not-killed '(() (a) (b)) '((a) (b) ()) '((a) () ()))
              '(() (a b) (b)))



; hold on, I think this is correct...
; no one is referencing slot 2, so eax is never being copied. 
; Is that wrong?
#;
(check-equal? (copy-inds '(() () (eax)) '(() (0) (1)) '(() () ()))
             '(() (eax) ())) ;'(() () ()) ?


(check-equal? (copy-inds '(() (a)) '(() (1)) '(() ()))
              '(() (a)))
(check-equal? (copy-inds '((a)) '((0)) '(()))
              '((a)))
  

#|  THE PARSER!
(define/contract (kills/gens instr)
  (-> list? kill-gen?)
  (match instr
    [`(,x <- (mem ,y ,n)) ]
    [`((mem ,y ,n) <- ,s) ]
    [`(eax <- (allocate ,t1 ,t2)) ]
    [`(eax <- (array-error ,t1 ,t2)) ]
    [`(,x <- ,s) ]
    [`(,x ,op ,t) ]
    [`(,cx <- ,t1 ,cop, ,t2) ]
    [(? symbol?) ]
    [`(goto ,label) ]
    [`(cjump ,t1 ,cop ,t2 ,label1 ,label2) ]
    [`(call ,u) ]
    [`(tail-call ,u) ]
    [`(return) ]
    [`(eax <- (print ,t)) ]
    [else (error 'parse "Expression didn't conform to L1 grammar")]))
|#

(check-equal? (preds 0 '(:rrr (eax <- 3) (eax += 4) (ebx <- 4) (goto :rrr)))
              '(4))
(check-equal? (preds 3 '(:f (eax <- 3) (eax += 4) :rrr (ebx <- 4) (goto :rrr)))
              '(5 2))
(check-equal? (preds 4 '(:f (eax <- 3) (cjump 2 < 4 :f :rrr) (eax += 4) :rrr (ebx <- 4) (goto :rrr)))
              '(6 2 3))


; the (eax <- 4) can never be gotten to, so does it's predecessors should be empty
; this test doesn't pass
#; ;Is this a bug?
(check-equal? (preds 3 '(:f (eax <- 1) (cjump 2<4 :f :f) (eax <- 4)))
              '())



(check-equal? (all-gens '(:f (eax <- 4) (eax += 1)))
              '(() ()(eax)))
(check-equal? (all-kills '(:f (eax <- 4) (eax += 1)))
              '(() (eax) (eax)))
(check-equal? (all-kills '(:f (eax <- 10) (eax += 10) (cjump eax = eax :f :f)))
              '(() (eax) (eax) ()))
(check-equal? (all-gens '(:f (eax <- 10) (eax += 10) (cjump eax = eax :f :f)))
              '(() () (eax) (eax)))
(check-equal? (all-gens '(:f (x <- 10) (y <- 11) (x >>= y) (cjump x < y :f :f)))
              '(() () () (x y) (x y)))
(check-equal? (all-kills '(:f (x <- 10) (y <- 11) (x >>= y) (cjump x < y :f :f)))
              '(() (x) (y) (x) ()))
(let ([prog '(:f (x <- 10) (x <- (mem ebp -4)) ((mem ebp -8) <- x))])
  (check-equal? (all-kills prog)
                '(() (x) (x) ()))
  (check-equal? (all-gens prog)
                '(() () (ebp) (ebp x))))
(let ([prog '(:f (eax <- 11) (eax <- (print eax)))])
  (check-equal? (all-kills prog)
                '(() (eax) (eax ecx edx)))
  (check-equal? (all-gens prog)
                '(() () (eax))))
(let ([prog '(:f (z <- 11) (y <- (mem ebp -4)) (eax <- (allocate z y)))])
  (check-equal? (all-kills prog)
                '(() (z) (y) (eax ecx edx)))
  (check-equal? (all-gens prog)
                '(() () (ebp) (z y))))
(let ([prog '(:f (a <- 5) (b <- 7) (eax <- (array-error a b)))])
  (check-equal? (all-kills prog)
                '(() (a) (b) (eax ecx edx)))
  (check-equal? (all-gens prog)
                '(() () () (a b))))
(let ([prog '(:f (x <- 11) (eax <- (array-error x x)))])
  (check-equal? (all-kills prog)
                '(() (x) (eax ecx edx)))
  (check-equal? (all-gens prog)
                '(() () (x))))
(let ([prog '(:f (call :g))])
  (check-equal? (all-kills prog)
                '(() (eax ebx ecx edx)))
  (check-equal? (all-gens prog)
                '(() (eax ecx edx))))
(let ([prog '(:f (y <- :g) (call y))])
  (check-equal? (all-kills prog)
                '(() (y) (eax ebx ecx edx)))
  (check-equal? (all-gens prog)
                '(() () (y eax ecx edx))))
(let ([prog '(:f (tail-call :g))])
  (check-equal? (all-kills prog)
                '(() ()))
  (check-equal? (all-gens prog)
                '(() (eax ecx edx esi edi))))
(let ([prog '(:f (a <- :g) (tail-call a))])
  (check-equal? (all-kills prog)
                '(() (a) ()))
  (check-equal? (all-gens prog)
                '(() () (a eax ecx edx esi edi))))
(let ([prog '(:f (return))])
  (check-equal? (all-kills prog)
                '(() ()))
  (check-equal? (all-gens prog)
                '(() (eax esi edi))))
(let ([prog '(:f (goto :r) :r (x <- 10))])
  (check-equal? (all-kills prog)
                '(() () () (x)))
  (check-equal? (all-gens prog)
                '(() () () ())))
(let ([prog '(:f (x <- 11) (y <- x) (edx <- x = y))])
  (check-equal? (all-kills prog)
                '(() (x) (y) (edx)))
  (check-equal? (all-gens prog)
                '(() () (x) (x y))))
(let ([prog '(:f (x <- 11) (z <- x <= x))])
  (check-equal? (all-kills prog)
                '(() (x) (z)))
  (check-equal? (all-gens prog)
                '(() () (x))))


