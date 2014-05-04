#lang racket
(require "liveness.rkt" rackunit racket/set)

(define all-registers '(eax ebx ecx edi edx esi))

(struct colored-node (color name) #:transparent)

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

(define (all-vars+regs prog)
  (sort (set-union (foldr set-union '() (all-kills prog)) 
             (foldr set-union '() (all-gens prog)) 
             '(eax ebx ecx edx esi edi)) symbol<?))

(define (interferes code)
   (let* ([var-list (all-vars+regs code)]
         [out-list (in-out-outs (in/out code))]
         [in-list  (in-out-ins (in/out code))]
         [kill-list (all-kills code)])
     (for/list ([v var-list])
       (cons v 
             (sort 
              (if (set-member? all-registers v)
                  (set-union (set-subtract all-registers (list v))
              (var-interferes v out-list in-list kill-list code))
              (var-interferes v out-list in-list kill-list code))
                   symbol<?)))))

(define (var-interferes var out-list in-list kill-list code)
  (let ([return-list
        (if (set-member? (first in-list) var)
            (first in-list)
            '())])
      (for ([out out-list]
            [killed kill-list]
            [instr code])
        (if (set-member? out var)
            (set! return-list (set-subtract (set-union out return-list killed) 
                                            (check-assigns instr var)))
            return-list)
        (if (set-member? killed var)
            (set! return-list (set-subtract (set-union out return-list)
                                            (check-assigns instr var)))
            return-list))
    (set-subtract return-list (list var))))

(define (check-assigns instr var)
  (match instr
    (`(,x <- ,y) (if (and (symbol? x) (symbol? y))
                     (cond [(symbol=? x var) (list y)]
                       [(symbol=? y var) (list x)]
                       [else '()])
                     '()))
    (else '())))

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
                 (set! newgraph (cons (colored-node i (first var-node))
                                      newgraph)))
          void))
    newgraph))

(define (check-neighbors colored-graph var-node i)
  (let ([neighbors (rest var-node)])
    (andmap (λ (neighb) (color-check colored-graph neighb i)) neighbors)
   ))

(define (color-check colored-graph neighbor num)
  (andmap (λ (cn) (not (and (equal? (colored-node-name cn) neighbor)
                            (equal? (colored-node-color cn) num))))
          colored-graph))

;(color-graph (interferes '(:f (eax <- 1) (eax += 2) (x += 5) (ebx <- 5))))

(if  (not (= (vector-length (current-command-line-arguments)) 1))
  (display "")
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (lambda (x) (pretty-out (read x)))))
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
