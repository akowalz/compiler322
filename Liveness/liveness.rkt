#lang racket

(struct kill-gen (kills gens))
(provide kills/gens)

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

(define/contract (kills/gens instr)
  (-> list? kill-gen?)
  (match instr
    [`(eax <- (print ,t)) (kill-gen '(eax ecx edx) (if (symbol? t)
                                               '(t)
                                               '()))]
    [`(,x <- (mem ,y ,n)) (kill-gen '(x) '(y))]
    [`((mem ,y ,n) <- ,s) (kill-gen '() (if (check-var-reg s) 
                                            '(y s) 
                                            '(y)))]
    [`(eax <- (allocate ,t1 ,t2)) (kill-gen '(eax) (two-ts t1 t2))]
    [`(eax <- (array-error ,t1 ,t2)) (kill-gen '(eax) (two-ts t1 t2))]
    [`(,x <- ,s) (kill-gen '(x) (if (check-var-reg s)
                                    '(s) 
                                    '()))]
    [`(,x ,op ,t) (kill-gen '(x) (if (symbol? t) 
                                     '(x t)
                                     '(x)))]
    [`(,cx <- ,t1 ,cop ,t2) (kill-gen '(cx) (two-ts t1 t2))]
    [(? symbol?) (kill-gen '() '())]
    [`(goto ,label) (kill-gen '() '())]
    [`(cjump ,t1 ,cop ,t2 ,label1 ,label2) (kill-gen '() (two-ts t1 t2))]
    [`(call ,u) (kill-gen '(ecx edx eax ebx) (if (not (label? u))
                                                 '(u eax ecx edx)
                                                 '(eax ecx edx)))]
    [`(tail-call ,u) (kill-gen '() (if (not (label? u))
                                       '(u eax ecx edx esi edi)
                                       '(eax ecx edx esi edi)))]
    [`(return) (kill-gen '() '(eax esi edi))]
    [else (error 'parse "Expression didn't conform to L1 grammar")]))
#;
(define/contract (killed? instr x)
  (-> list? symbol? boolean?)
  (kill-gen-kills (kills/gens instr)))

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

(define/contract (label? x)
  (-> (or/c symbol? number?) boolean?)
  (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" x))
    
