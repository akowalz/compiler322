#lang racket
(require rackunit)

(define (generate-spilled-program path)
  (let* ([args (call-with-input-file path (lambda (x) (list (read x) (read x) (read x) (read x))))]
         [program (first args)]
         [var (second args)]
         [addr (third args)]
         [prefix (fifth args)])
    (display (spill-program program var addr prefix))))
    

(define (spill-program instrs var addr prefix)
  (string-append "(" (spill-program-int instrs var addr prefix 0) ")"))
                        

(define (spill-program-int instrs var addr prefix count)
  (if (empty? instrs) ""
      (let* ([result (spill-instr (first instrs) var addr prefix count)]
             [new-count (aSpill-count result)]
             [result-expr (aSpill-expr result)])
        (string-append result-expr "\n"
                       (spill-program-int (rest instrs) var addr prefix new-count)))))

(struct aSpill (expr count))

(define (spill-instr ins var addr prefix count)
  (if (not (includes ins var)) (aSpill (list->string ins) count) 
      (match ins
        [`(,x <- (mem ,dest ,n)) (let ([temp (new-temp count prefix)])
                                   (aSpill (string-append (put-in-temp temp addr) 
                                                          (cond [(and (equal? var x) (equal? x dest)
                                                                      (format "(~A <- (mem ~A ~A))\n((mem ebp ~A) <- ~A)"
                                                                              temp temp n addr temp))]
                                                                [(equal? var x) 
                                                                 (format "(~A <- (mem ~A ~A))\n((mem ebp ~A) <- ~A)"
                                                                         temp dest n addr temp)]
                                                                [(equal? var dest)
                                                                 (format "(~A <- (mem ~A ~A))"
                                                                         x temp n)])) (+ 1 count)))]
        [`((mem ,dest ,n) <- ,x) (let ([temp (new-temp count prefix)])
                                   (aSpill (string-append (put-in-temp temp addr)
                                                          (cond [(and (equal? var x) (equal? x dest)
                                                                      (format "((mem ~A ~A) <- ~A)"
                                                                              temp n temp))]
                                                                [(equal? var x) 
                                                                 (format "((mem ~A ~A) <- ~A)"
                                                                         dest n temp)]
                                                                [(equal? var dest)
                                                                 (format "((mem ~A ~A) <- ~A)"
                                                                         temp n x)])) (+ 1 count)))]
        [`(eax <- (print ,t)) (let ([temp (new-temp count prefix)])
                                (aSpill (string-append (put-in-temp temp addr)
                                                       (format "(eax <- (print ~A))"
                                                               temp))
                                        (+ count 1)))]
        [`(eax <- (,runtime ,t1 ,t2)) (let ([temp (new-temp count prefix)])
                                        (aSpill (string-append (put-in-temp temp addr)
                                                               (format "(eax <- (~A "
                                                                       runtime)
                                                               (cond [(equal? t1 t2) (format "~A ~A))"
                                                                               temp temp)]
                                                                     [(equal? t1 var) (format "~A ~A))"
                                                                                temp t2)]
                                                                     [(equal? t2 var) (format "~A ~A))"
                                                                                              t1 temp)]))
                                                (+ 1 count)))]
        [`(,cx <- ,t1 ,cop ,t2) (let ([temp (new-temp count prefix)])
                                  (if (and (not (or (equal? cx t1)
                                                    (equal? cx t2)))
                                           (equal? cx var))
                                      (aSpill (format "(~A <- ~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                              temp t1 cop t2 addr temp) (+ 1 count))
                                      (aSpill (string-append (put-in-temp temp addr)
                                                     (cond [(and (equal? cx var) (equal? t2 cx) (equal? t1 cx)) 
                                                            (format "(~A <- ~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                                                    temp temp cop temp addr temp)]
                                                           [(and (equal? cx var) (equal? t1 cx)) 
                                                            (format "(~A <- ~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                                                    temp temp cop t2 addr temp)]
                                                           [(and (equal? cx var) (equal? t2 cx)) 
                                                            (format "(~A <- ~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                                                    temp t1 cop temp addr temp)]
                                                           [(equal? t1 var) 
                                                            (format "(~A <- ~A ~A ~A)"
                                                                    cx temp cop t2)]
                                                           [(equal? t2 var)
                                                            (format "(~A <- ~A ~A ~A)"
                                                                    cx t1 cop temp)]))
                                      (+ 1 count))))]
        [`(,v <- ,x) (cond [(and (equal? x var) (equal? x v)) (aSpill "" count)]
                           [(equal? v var) (aSpill (format "((mem ebp ~A) <- ~A)"
                                                           addr x)
                                                   count)]
                           [(equal? x var) (aSpill (format "(~A <- (mem ebp ~A))"
                                                           v addr)
                                                   count)])]
        [`(,v ,op ,x) (let ([temp (new-temp count prefix)])
                        (aSpill 
                         (string-append 
                          (put-in-temp temp addr)
                          (cond [(and (equal? x var) (equal? v var))
                                 (format "(~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                         temp op temp addr temp)]
                                [(equal? v var) (format "(~A ~A ~A)\n((mem ebp ~A) <- ~A)"
                                                        temp op x addr temp)]
                                [(equal? x var) (format "(~A ~A ~A)"
                                                         v op temp)])) (+ 1 count)))]
        [`(cjump ,a ,cmp ,b ,l1 ,l2) (let ([temp (new-temp count prefix)])
                                       (aSpill (string-append 
                                                (put-in-temp temp addr)
                                                (cond [(and (equal? a var) (equal? b var))
                                                       (format "(cjump ~A ~A ~A ~A ~A)"
                                                               temp cmp temp l1 l2)]
                                                      [(equal? a var)
                                                       (format "(cjump ~A ~A ~A ~A ~A)"
                                                              temp cmp b l1 l2)]
                                                      [(equal? b var)
                                                       (format "(cjump ~A ~A ~A ~A ~A)"
                                                               a cmp temp l1 l2)])) (+ 1 count)))]
        
        ['() ""]
        [else "error"])))

(define (thing->string lst)
  (cond [(list? lst) (list->string lst)]
        [(symbol? lst) (symbol->string lst)]
        [(number? lst) (number->string lst)]
        [(string? lst) lst]))

(define (list->string lst)
  (format "(~A)"
          (string-join (map thing->string lst) " ")))

(define (put-in-temp temp offset)
  (string-append (list->string (list temp '<- (list 'mem 'ebp offset))) "\n"))

(check-equal? (put-in-temp 'x0 -4) "(x0 <- (mem ebp -4))\n")
  
(define (new-temp count sym)
  (string-append (thing->string sym)
                 (thing->string count)))


(define (includes lst sym)
  (cond [(empty? lst) #f]
        [(list? (first lst)) (or (includes (first lst) sym)
                                 (includes (rest lst) sym))]
        [else (or (equal? (first lst) sym)
                  (includes (rest lst) sym))]))
                                
      

(check-equal? (includes '(a b c) 'a) #t)
(check-equal? (includes '(a b d) 'c) #f)
(check-equal? (includes '(a b (c d r)) 'r) #t)




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
(check-equal? (aSpill-expr (spill-instr '(v <- v) 'v -4 'r 0))
              "")

;(check-equal? (aSpill-expr (spill-exprs '((a <- 5)) 'a -4 's))
              ;"((mem ebp -4) <- 5)")
;Mem arrows
(check-equal? (aSpill-expr (spill-instr '((mem ebp 8) <- x) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n((mem ebp 8) <- s0)")
(check-equal? (aSpill-expr (spill-instr '((mem x 8) <- x) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n((mem s0 8) <- s0)")
(check-equal? (aSpill-expr (spill-instr '(eax <- (mem x 8)) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(eax <- (mem s0 8))")
(check-equal? (aSpill-expr (spill-instr '(x <- (mem ebp 8)) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(s0 <- (mem ebp 8))\n((mem ebp -4) <- s0)")

; Ops
(check-equal? (aSpill-expr (spill-instr '(x += x) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(s0 += s0)\n((mem ebp -4) <- s0)")

;t cmp t
(check-equal? (aSpill-expr (spill-instr '(eax <- x < 4) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(eax <- s0 < 4)")
(check-equal? (aSpill-expr (spill-instr '(v *= x) 'x -4 's 1))
              "(s1 <- (mem ebp -4))\n(v *= s1)")
(check-equal? (aSpill-expr (spill-instr '(v >>= e) 'b -4 's 1))
              "(v >>= e)")

(check-equal? (aSpill-expr (spill-instr '(x <- 5 < 4) 'x -4 's 0))
              "(s0 <- 5 < 4)\n((mem ebp -4) <- s0)")
(check-equal? (aSpill-expr (spill-instr '(x <- x < 4) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(s0 <- s0 < 4)\n((mem ebp -4) <- s0)")
(check-equal? (aSpill-expr (spill-instr '(x <- x < x) 'x -4 't 0))
              "(t0 <- (mem ebp -4))\n(t0 <- t0 < t0)\n((mem ebp -4) <- t0)")
(check-equal? (aSpill-expr (spill-instr '(x *= 4) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(s0 *= 4)\n((mem ebp -4) <- s0)")


;cjumps 
(check-equal? (aSpill-expr (spill-instr '(cjump s = 4 :here :there) 's -4 'x 0))
              "(x0 <- (mem ebp -4))\n(cjump x0 = 4 :here :there)")
(check-equal? (aSpill-expr (spill-instr '(cjump s < s :here :there) 's -4 'x 0))
              "(x0 <- (mem ebp -4))\n(cjump x0 < x0 :here :there)")
(check-equal? (aSpill-expr (spill-instr '(cjump 4 <= s :here :there) 's -4 'x 0))
              "(x0 <- (mem ebp -4))\n(cjump 4 <= x0 :here :there)")

;runtime calls

;print
(check-equal? (aSpill-expr (spill-instr `(eax <- (print x)) 'x -8 's 1))
              "(s1 <- (mem ebp -8))\n(eax <- (print s1))")
(check-equal? (aSpill-expr (spill-instr `(eax <- (print x)) 'y -4 's 2))
              "(eax <- (print x))")

;others
(check-equal? (aSpill-expr (spill-instr `(eax <- (allocate a b)) 'a -4 's 0))
              "(s0 <- (mem ebp -4))\n(eax <- (allocate s0 b))")
(check-equal? (aSpill-expr (spill-instr `(eax <- (allocate 'x 'x)) 'x -4 's 0))
              "(s0 <- (mem ebp -4))\n(eax <- (allocate s0 s0))")

;spill-program
(check-equal? (spill-program '((v1 <- 5)) 'v1 -4 's)
              "(((mem ebp -4) <- 5)\n)")
(check-equal? (spill-program '((v1 <- 5) (v1 += 7)) 'v1 -4 's)
              "(((mem ebp -4) <- 5)\n(s0 <- (mem ebp -4))\n(s0 += 7)\n((mem ebp -4) <- s0)\n)")
(check-equal? (spill-program '((x <- 1) (x += x) (eax <- x)) 'x -4 's)
              "(((mem ebp -4) <- 1)\n(s0 <- (mem ebp -4))\n(s0 += s0)\n((mem ebp -4) <- s0)\n(eax <- (mem ebp -4))\n)")
(check-equal? (spill-program '((x <- 21)
                               (y <- 31)
                               (eax <- x = y)
                               (x += 10)
                               (eax <- x = y)) 
                             'x -4 's)
              (string-append "(" (string-join 
                                  (list
                                   "((mem ebp -4) <- 21)"
                                   "(y <- 31)"
                                   "(s0 <- (mem ebp -4))"
                                   "(eax <- s0 = y)"
                                   "(s1 <- (mem ebp -4))"
                                   "(s1 += 10)"
                                   "((mem ebp -4) <- s1)"
                                   "(s2 <- (mem ebp -4))"
                                   "(eax <- s2 = y)")
                                  "\n")
                             "\n)"))

;(generate-spilled-program (vector-ref (current-command-line-arguments) 0))
                             

                    
