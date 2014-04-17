#lang racket

(struct kill-gen (kills gens))
(provide kills/gens)

(define/contract (label? x)
  (-> (or/c symbol? number? list?) boolean?)
  (if (symbol? x)
      (if (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string x))
          #t
          #f)
      #f))

(define/contract (kills/gens instr)
  (-> list? kill-gen?)
  (match instr
    [`(eax <- (print ,t)) (kill-gen `(eax ecx edx) (if (symbol? t)
                                                       (list t)
                                                       (list )))]
    [`(,x <- (mem ,y ,n)) (kill-gen (list x) (list y))]
    [`((mem ,y ,n) <- ,s) (kill-gen (list ) (if (check-var-reg s) 
                                                (list y s) 
                                                (list y)))]
    [`(eax <- (allocate ,t1 ,t2)) (kill-gen `(eax) (two-ts t1 t2))]
    [`(eax <- (array-error ,t1 ,t2)) (kill-gen `(eax) (two-ts t1 t2))]
    [`(,x <- ,s) (kill-gen (list x) (if (check-var-reg s)
                                        (list s) 
                                        (list)))]
    [`(,x ,op ,t) (kill-gen (list x) (if (symbol? t) 
                                         (list x t)
                                         (list x)))]
    [`(,cx <- ,t1 ,cop ,t2) (kill-gen (list cx) (two-ts t1 t2))]
    [(? symbol?) (kill-gen (list ) (list ))]
    [`(goto ,label) (kill-gen (list ) (list ))]
    [`(cjump ,t1 ,cop ,t2 ,label1 ,label2) (kill-gen (list ) (two-ts t1 t2))]
    [`(call ,u) (kill-gen `(ecx edx eax ebx) (if (not (label? u))
                                                 `(,u eax ecx edx)
                                                 `(eax ecx edx)))]
    [`(tail-call ,u) (kill-gen (list ) (if (not (label? u))
                                           `(,u eax ecx edx esi edi)
                                           `(eax ecx edx esi edi)))]
    [`(return) (kill-gen (list ) `(eax esi edi))]
    [else (error 'parse "Expression didn't conform to L1 grammar")]))
#;
(define/contract (killed? instr x)
  (-> list? symbol? boolean?)
  (kill-gen-kills (kills/gens instr)))

(define/contract (preds num func)
  (-> number? list? list?)
  (let ([instr (list-ref func num)])
    (if (= num 0) 
        (find-refs instr func)
        (if (label? instr)
            (append (find-refs instr func) (list (- num 1)))
            (list (- num 1))))))

(define/contract (find-refs label func)
  (-> label? list? list?)
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

  (-> (or/c symbol? number?)
      (or/c symbol? number?)
      list?)

  (cond
    [(and (symbol? t1) (symbol? t2))
     '(t1 t2)]
    [(symbol? t1) '(t1)]
    [(symbol? t2) '(t2)]
    [else '()]))

(define/contract (in/out )
  )

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

(preds 0 '(:rrr (eax <- 3) (eax += 4) (ebx <- 4) (goto :rrr)))
(preds 3 '(:f (eax <- 3) (eax += 4) :rrr (ebx <- 4) (goto :rrr)))
(preds 4 '(:f (eax <- 3) (cjump 2 < 4 :f :rrr) (eax += 4) :rrr (ebx <- 4) (goto :rrr)))
