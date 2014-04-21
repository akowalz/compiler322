#lang racket
(require racket/set)
(require rackunit)
(require racket/stream)

(struct kill-gen (kills gens))
(struct in-out (ins outs))
;(provide kills/gens)


(define/contract (label? x)
  (-> (or/c symbol? number? list?) boolean?)
  (and (symbol? x)
       (regexp-match? #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$"
                      (symbol->string x))))

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

; FIXME
; think about changing the last line to see if previous instruction was a jump, goto, call, etc
(define/contract (preds num func)
  (-> number? (or/c list? symbol?) (listof number?))
  (let ([instr (list-ref func num)])
    (remove-duplicates
     (if (= num 0) 
         '()
         (if (label? instr)
             (append (find-refs instr func) 
                     (if (stops-control-flow? (- num 1) func) (list )
                         (list (- num 1))))
             (if (stops-control-flow? (- num 1) func) (list )
                 (list (- num 1))))))))

(define/contract (stops-control-flow? index func)
  (-> number? list? boolean?)
  (let ([instr (list-ref func index)])
    (match instr 
      [`(cjump ,_ ,_ ,_ ,_ ,_) #t]
      [`(goto ,_) #t]
      [`(return) #t]
      [`(tail-call ,_) #t]
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
  
#; 
(define/contract (copy-not-killed ins outs kill-list)
  (-> list? list? list? list?)
  (map set-subtract (copy-inds outs (make-list-of-increasing-ints (length ins)) ins)
       kill-list))

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


; and we might not need this
#|
(define (make-list-of-increasing-ints n)
  (if (= n 1) '((0)) 
      (append (make-list-of-increasing-ints (- n 1)) (list (list (- n 1))))))

(check-equal? (make-list-of-increasing-ints 3)
              '((0) (1) (2)))
(check-equal? (make-list-of-increasing-ints 1)
              '((0)))
|#

;; wtfomgbbq
(define/contract (copy-inds src indexes dst)
  (-> (listof (listof symbol?))
      (listof (listof number?))
      (listof (listof symbol?))
      list?) ; functional programming, man
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

(provide (all-defined-out))





