#lang racket
(require rackunit)


(define (spill-exprs exprs var addr prefix)
  (let ([count 0])
  (foldr string-append (spill-instr (first exprs) var addr prefix)
         (spill-instr (rest exprs) var addr prefix))))

(struct aSpill (expr count))
  
  

(define (spill-instr ins var addr prefix count)
  (if (not (includes ins var)) (aSpill (list->string ins) count) 
      (match ins
        [`(,v <- ,x) (cond [(and (equal? x var) (equal? x v)) ""]
                           [(equal? v var) (aSpill (format "((mem ebp ~A) <- ~A)"
                                                   addr x)
                                                   count)]
                           [(equal? x var) (aSpill (format "(~A <- (mem ebp ~A))"
                                                   v addr)
                                                   count)])]
        [`(,v ,op ,x) (let ([temp (new-temp count prefix)])
                        (aSpill (cond [(and (equal? x var) (equal? v var))
                                       (format "(~A <- (mem ebp ~A))\n(~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                               temp addr temp op temp addr temp)]
                                      [(equal? v var) (format "(~A <- (mem ebp ~A))\n(~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                                              temp addr temp op x addr temp)]
                                      [(equal? x var) (format "(~A <- (mem ebp ~A))\n(~A ~A ~A)"
                                                              temp addr v op temp)]) (+ 1 count)))]
        [`(cjump ,a ,cmp ,b ,l1 ,l2) (let ([temp (new-temp count prefix)])
                                      (aSpill (string-append 
                                               (format "(~A <- (mem ebp ~A))\n"
                                                       temp addr)
                                               (cond [(and (equal? a var) (equal? b var))
                                                      (format "(cjump ~A ~A ~A ~A ~A)"
                                                              temp cmp temp l1 l2)]
                                                     [(equal? a var)
                                                      (format "(cjump ~A ~A ~A ~A ~A)"
                                                              temp cmp b l1 l2)]
                                                     [(equal? b var)
                                                      (format "(cjump ~A ~A ~A ~A ~A)"
                                                              a cmp temp l1 l2)])) (+ 1 count)))]
        [`(eax <- (print ,t)) (let ([temp (new-temp count prefix)])
                                (aSpill (format "(~A <- (mem ebp -4))\n(eax <- (print ~A))"
                                                temp addr temp)
                                        (+ count 1)))]
        ;[`(eax <- (,runtime2 ,t1 ,t2))
        ['() ""])))
  
(define (new-temp count sym)
  (string-append (thing->string sym)
                 (thing->string count)))

(define (includes lst sym)
  (if (empty? lst) #f
      (or (equal? sym (first lst))
          (includes (rest lst) sym))))

(check-equal? (includes '(a b c) 'a) #t)
(check-equal? (includes '(a b d) 'c) #f)

(define (thing->string lst)
  (cond [(list? lst) (list->string lst)]
        [(symbol? lst) (symbol->string lst)]
        [(number? lst) (number->string lst)]
        [(string? lst) lst]))

(define (list->string lst)
  (format "(~A)"
          (string-join (map thing->string lst) " ")))

(check-equal? (list->string '(a b c)) "(a b c)")
(check-equal? (list->string '((mem ebp -4) <- eax))
              "((mem ebp -4) <- eax)")
         

; Basic arrows
(check-equal? (aSpill-expr (spill-instr '(a <- 5) 'a -4 's 0))
           "((mem ebp -4) <- 5)")

(check-equal? (aSpill-expr (spill-instr '(a <- 5) 'v -4 's 0))
           "(a <- 5)")

(check-equal? (aSpill-expr (spill-instr '(eax <- s) 's -4 'r 0))
              "(eax <- (mem ebp -4))")

;(check-equal? (aSpill-expr (spill-exprs '((a <- 5)) 'a -4 's))
              ;"((mem ebp -4) <- 5)")

; Ops
(check-equal? (aSpill-expr (spill-instr '(x += x) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(s0 += s0)\n((mem ebp -4) <- s0)")
(check-equal? (aSpill-expr (spill-instr '(x *= 4) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(s0 *= 4)\n((mem ebp -4) <- s0)")
(check-equal? (aSpill-expr (spill-instr '(v *= x) 'x -4 's 1))
              "(s1 <- (mem ebp -4))\n(v *= s1)")
(check-equal? (aSpill-expr (spill-instr '(v >>= e) 'b -4 's 1))
              "(v >>= e)")

;cjumps 
(check-equal? (aSpill-expr (spill-instr '(cjump s = 4 :here :there) 's -4 'x 0))
              "(x0 <- (mem ebp -4))\n(cjump x0 = 4 :here :there)")
(check-equal? (aSpill-expr (spill-instr '(cjump s < s :here :there) 's -4 'x 0))
              "(x0 <- (mem ebp -4))\n(cjump x0 < x0 :here :there)")
(check-equal? (aSpill-expr (spill-instr '(cjump 4 <= s :here :there) 's -4 'x 0))
              "(x0 <- (mem ebp -4))\n(cjump 4 <= x0 :here :there)")
                    
