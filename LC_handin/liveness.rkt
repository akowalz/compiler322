#lang racket
(require racket/set)
(require rackunit)


; Data
(struct kill-gen (kills gens))
(struct in-out (ins outs))


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
                                        '()))]
    [`(,x ,op ,t) (kill-gen (list x) (if (symbol? t) 
                                         (list x t)
                                         (list x)))]
    [`(,cx <- ,t1 ,cop ,t2) (kill-gen (list cx) (two-ts t1 t2))]
    [(? symbol?) (kill-gen '() '())]
    [`(goto ,label) (kill-gen '() '())]
    [`(cjump ,t1 ,cop ,t2 ,label1 ,label2) (kill-gen '() (two-ts t1 t2))]
    [`(call ,u) (kill-gen `(eax ebx ecx edx) (if (not (label? u))
                                                 `(,u eax ecx edx)
                                                 `(eax ecx edx)))]
    [`(tail-call ,u) (kill-gen '() (if (not (label? u))
                                           `(,u eax ecx edx esi edi)
                                           `(eax ecx edx esi edi)))]
    [`(return) (kill-gen '() `(eax esi edi))]
    [else (error 'parse "Expression didn't conform to L1 grammar")]))

(define/contract (label? x)
  (-> (or/c symbol? number? list?) boolean?)
  (and (symbol? x)
       (regexp-match? #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$"
                      (symbol->string x))))

(define (kills/gens-program prog)
  (map kills/gens prog))

(define (all-kills prog)
  (map kill-gen-kills (kills/gens-program prog)))

(define (all-gens prog)
  (map kill-gen-gens (kills/gens-program prog)))

(define/contract (preds num func)
  (-> number? (or/c list? symbol?) (listof number?))
  (let ([instr (list-ref func num)])
    (remove-duplicates
     (if (= num 0) 
         '()
         (if (label? instr)
             (append (find-refs instr func) 
                     (if (stops-control-flow? (- num 1) func)
                         '()
                         (list (- num 1))))
             (if (stops-control-flow? (- num 1) func)
                 '()
                 (list (- num 1))))))))

(define/contract (stops-control-flow? index func)
  (-> number? list? boolean?)
  (let ([instr (list-ref func index)])
    (match instr 
      [`(cjump ,_ ,_ ,_ ,_ ,_) #t]
      [`(goto ,_) #t]
      [`(return) #t]
      [`(tail-call ,_) #t]
      [`(eax <- (array-error ,_ ,_)) #t]
      [else #f])))

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

(define/contract (all-preds func)
  (-> list? (listof (listof number?)))
  (map (lambda (n) (preds n func))
       (stream->list (in-range (length func)))))

(define/contract (check-var-reg s)
  (-> (or/c symbol? number?) boolean?)
  (and (symbol? s)
       (not (label? s))))

(define/contract (two-ts t1 t2)
  (-> (or/c symbol? number?) (or/c symbol? number?) list?)
  (cond
    [(and (symbol? t1) (symbol? t2))
     (if (symbol=? t1 t2) (list t1)
         (list t1 t2))]
    [(symbol? t1) (list t1)]
    [(symbol? t2) (list t2)]
    [else '()]))

(define/contract (copy-not-killed ins outs kill-list)
  (-> (listof (listof symbol?))
      (listof (listof symbol?))
      (listof (listof symbol?))
      list?)
  (map set-union (map set-subtract outs kill-list) ins))

(define (make-list-of-empties n)
  (if (= n 0) '()
      (cons empty (make-list-of-empties (- n 1)))))

;; wtfomgbbq
(define/contract (copy-inds src indexes dst)
  (-> (listof (listof symbol?))
      (listof (listof number?))
      (listof (listof symbol?))
      list?)
  (for/list ([is indexes]
             [j (in-range (length dst))])
    (set-union (list-ref dst j)
               (foldr set-union '()
                      (map (lambda (n) (list-ref src n)) is)))))
   
   
(define/contract (includes? lst n)
  (-> (listof number?) number? boolean?)
  (if (empty? lst) #f
      (or (= (car lst) n)
          (includes? (rest lst) n))))

(define/contract (transform-indexes indexes)
  (-> (listof (listof number?))
      (listof (listof number?)))
  (for/list ([i (in-range (length indexes))])
    (let ([result empty])
      (for ([p indexes]
            [j (in-range (length indexes))])
        (if (includes? p i)
            (set! result (cons j result))
            result))
      result)))

(define successors (λ (i) (transform-indexes (all-preds i))))

(define/contract (in/out func)
  (-> list? in-out?)
  (let* ([kill-list (all-kills func)]
         [pred-list (successors func)]
         [ins (all-gens func)]
         [outs (copy-inds ins pred-list (make-list-of-empties (length func)))])
    (in/out-help ins outs kill-list pred-list))) 

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
     
(define (in/out-pretty fun)
  (let ([ios (in/out fun)])
    (list (cons 'in (map (λ (lst) (sort (set-subtract lst '(esp ebp))
                                        symbol<?)) (in-out-ins ios)))
          (cons 'out (map (λ (lst) (sort (set-subtract lst '(esp ebp))
                                         symbol<?)) (in-out-outs ios))))))
#|
(if  (not (= (vector-length (current-command-line-arguments)) 1))
  (display "")
  (display (call-with-input-file
               (vector-ref (current-command-line-arguments) 0)
             (lambda (x) (in/out-pretty (read x))))))
|#
  
(provide (all-defined-out))





