#lang racket
(require "liveness.rkt" rackunit racket/set)

(define all-registers '(eax ebx ecx edi edx esi))

(struct colored-node (color name) #:transparent)

(define (remove-stack-vars lst)
  (set-subtract lst '(ebp esp)))

(define (pretty-out code)
  (let* ([i-graph (interferes code)]
         [temp-c (color-graph i-graph)]
         [c-graph (if (set-member? temp-c #f)
                      #f
                      (filter (λ (v) (not (set-member? all-registers (colored-node-name v))))
                              temp-c))])
    (display i-graph)
    (display "\n")
    (display (if c-graph
                 (map (λ (node) (list (colored-node-name node)
                         (list-ref all-registers
                                   (colored-node-color node))))
                      c-graph)
                 #f))))

(define (color-mapping code)
  (let* ([i-graph (interferes code)]
         [temp-c (color-graph i-graph)])
    (if (set-member? temp-c #f)
        #f
        (let ([nodes (filter (λ (v) (not (set-member? all-registers (colored-node-name v))))
                             temp-c)])
          (make-hash (map (λ (node) (cons (colored-node-name node) (list-ref all-registers 
                                                                             (colored-node-color node))))
                          nodes))))))
    

(define (all-vars+regs prog)
  (sort (set-subtract 
         (set-union (foldr set-union '() (all-kills prog)) 
                    (foldr set-union '() (all-gens prog)) 
                    '(eax ebx ecx edx esi edi))
         '(ebp esp))
        symbol<?))

(define (all-vars prog)
  (sort (set-subtract (set-union (foldr set-union '() (all-kills prog)) 
                                 (foldr set-union '() (all-gens prog)))
                      '(eax ebx ecx edx esi edi ebp esp))
        symbol<?))

(define (interferes code)
   (let* ([var-list (all-vars+regs code)]
         [out-list (in-out-outs (in/out code))]
         [in-list  (in-out-ins (in/out code))]
         [kill-list (all-kills code)]) 
     (for/list ([v var-list])
       (cons v 
             (sort 
              (remove-stack-vars 
               (if (set-member? all-registers v)
                   (set-union (set-subtract all-registers (list v))
                              (var-interferes v out-list in-list kill-list code))
                   (var-interferes v out-list in-list kill-list code)))
              symbol<?)))))

(define (var-interferes var out-list in-list kill-list code)
  (let ([return-list
         (cond [(empty? in-list) '()]
                [(set-member? (first in-list) var)
                 (first in-list)]   
                [#t '()])])
    (set! return-list (foldr set-union return-list 
                             (for/list ([out out-list]
                                        [killed kill-list]
                                        [instr code])
                               (set-union (get-shift-edges instr var)
                                          (get-cmp-edges instr var)
                                          (if (set-member? out var)
                                              (set-subtract (set-union out killed) 
                                                            (check-assigns instr var))
                                              '())
                                          (if (set-member? killed var)
                                              (set-subtract out
                                                            (check-assigns instr var))
                                              '())))))
    (set-subtract return-list (list var))))

(define (check-assigns instr var)
  (match instr
    (`(,x <- ,y) (if (and (symbol? x) (symbol? y))
                     (cond [(symbol=? x var) (list y)]
                           [(symbol=? y var) (list x)]
                           [else '()])
                     '()))
    (else '())))

(define (get-shift-edges instr var)
   (match instr
     [`(,x <<= ,y) (cond 
                     [(not (symbol? y)) '()]
                     [(symbol=? y var)
                      '(eax ebx edx esi edi)]
                     [(set-member? '(eax ebx edx esi edi) var) 
                      (list y)]
                     [else '()])]
      [`(,x >>= ,y) (cond
                      [(not (symbol? y)) '()]
                      [(symbol=? y var)
                       '(eax ebx edx esi edi)]
                      [(set-member? '(eax ebx edx esi edi) var) 
                       (list y)]
                      [else '()])]
     [else '()]))

(define (get-cmp-edges instr var)
  (match instr
    [`(,t0 <- ,_ ,_ ,_) (cond
                          [(not (symbol? t0)) '()]
                          [(symbol=? var t0)
                               '(esi edi)]
                          [(set-member? '(esi edi) var)
                           (list t0)]
                          [else '()])]
    [else '()]))
  
                       

(define (color-graph graph)
  (let* ([colored-graph '()]
         [stack '()])
    ;Take out all non-register variables and put in stack
    (begin (for ([var-node graph])
             (if (set-member? all-registers (first var-node))
                 void
                 (set! stack (cons var-node stack))))
    ;Build colored graph with registers
    (for ([register all-registers]
          [i (in-range 6)])
      (set! colored-graph (cons (colored-node i register) colored-graph)))
    ;Add back in variables to colored graph
    (for ([var-node stack])
      (let ([new-graph (att-to-color colored-graph var-node)])
        (if (equal? new-graph colored-graph)
            ;failed to color
            (set! colored-graph (cons #f colored-graph))
            ;succeeded to color
            (set! colored-graph new-graph))))
    colored-graph)))

(define (att-to-color colored-graph var-node)
  (let ([newgraph colored-graph]
        [succeeded #f])
    (for [(i (in-range 6))]
      (if (and (not succeeded)
               (check-neighbors colored-graph var-node i))
          (begin (set! succeeded #t)
                 (set! newgraph (cons (colored-node i
                                                    (first var-node))
                                      newgraph)))
          void))
    newgraph))

(define (check-neighbors colored-graph var-node i)
  (let ([neighbors (rest var-node)])
    (andmap (λ (neighb) (color-check colored-graph neighb i)) neighbors)))

(define (color-check colored-graph neighbor num)
  (if (not (set-member? colored-graph #f))
      (andmap (λ (cn) (not (and (equal? (colored-node-name cn) neighbor)
                                (equal? (colored-node-color cn) num))))
              colored-graph)
      #f))
#|
(if  (not (= (vector-length (current-command-line-arguments)) 1))
  (display "")
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (lambda (x) (pretty-out (read x)))))
|#

(provide (all-defined-out))

#|(check-equal? (var-interferes 'x '((y z) (x t)) '(() ()) '(()()))
              '(t))
(check-equal? (var-interferes 'x '((y z) (x t)) '(() ()) '((x)()))
              '(t y z))
(check-equal? (var-interferes 'x '((x y z) (x t)) '((x ins) ()) '(()()))
              '(t ins y z))|#

(check-equal? (interferes '(:g
(rx <- eax)
(rx += ebx) (rx += ecx) (rx += edx)
(rx += edi) (rx += esi) (rx += eax))
)
              '((eax ebx ecx edi edx esi rx)
(ebx eax ecx edi edx esi rx)
(ecx eax ebx edi edx esi rx)
(edi eax ebx ecx edx esi rx)
(edx eax ebx ecx edi esi rx)
(esi eax ebx ecx edi edx rx)
(rx eax ebx ecx edi edx esi)))

(check-equal? (interferes '(:f
                           (eax <<= x)))
              '((eax ebx ecx edi edx esi x)
                (ebx eax ecx edi edx esi x)
                (ecx eax ebx edi edx esi)
                (edi eax ebx ecx edx esi x)
                (edx eax ebx ecx edi esi x)
                (esi eax ebx ecx edi edx x)
                (x eax ebx edi edx esi)))

(check-equal? (interferes '(:g (y <- 2) (x <- 1) (x += y) (y <- x)))
              '((eax ebx ecx edi edx esi)
                (ebx eax ecx edi edx esi)
                (ecx eax ebx edi edx esi)
                (edi eax ebx ecx edx esi)
                (edx eax ebx ecx edi esi)
                (esi eax ebx ecx edi edx)
                (x y)
                (y x)))


(check-equal? (interferes '((eax <- s3)
                            (eax <- s2)
                            (eax <- s1)
                            (eax <- s0)
                            (eax <- (array-error s0 s1))
                            (s3 <- 0)
                            (s2 <- 1)
                            (s1 <- 2)
                            (s0 <- 3)))
              '((eax ebx ecx edi edx esi s0 s1 s2)
                (ebx eax ecx edi edx esi)
                (ecx eax ebx edi edx esi)
                (edi eax ebx ecx edx esi)
                (edx eax ebx ecx edi esi)
                (esi eax ebx ecx edi edx)
                (s0 eax s1 s2 s3)
                (s1 eax s0 s2 s3)
                (s2 eax s0 s1 s3)
                (s3 s0 s1 s2)))
(check-equal? (interferes '((eax <- 5)
                            (ebx <- eax)
                            (ecx <- ebx)
                            (edx <- ecx)
                            (eax <- (print x))
                            (x <- edx)))
              ' ((eax ebx ecx edi edx esi x)
                 (ebx eax ecx edi edx esi x)
                 (ecx eax ebx edi edx esi x)
                 (edi eax ebx ecx edx esi)
                 (edx eax ebx ecx edi esi x)
                 (esi eax ebx ecx edi edx)
                 (x eax ebx ecx edx)))

(check-equal? (interferes '((eax <- (mem ebp -12))))
             '((eax ebx ecx edi edx esi)
               (ebx eax ecx edi edx esi)
               (ecx eax ebx edi edx esi) 
               (edi eax ebx ecx edx esi)
               (edx eax ebx ecx edi esi)
               (esi eax ebx ecx edi edx)))

