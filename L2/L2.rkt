#lang racket
(require "Graph.rkt" "spill.rkt" "liveness.rkt" rackunit)

(define/contract (L2->L1 code)
  (-> list? list?)
  (map L2f->L1f code))

(define/contract (L2f->L1f fun)
  (-> list? list?)
  (let* ([colors-hash (color-mapping fun)]
         [vars (all-vars fun)])
    (if colors-hash
        (replace-vars-with-regs fun colors-hash)
        (L2f->L1f-internal fun 0 vars))))

(define (L2f->L1f-internal fun attempt vars)
  (let* ([colors-hash (color-mapping fun)])
    (if colors-hash
        (let* ([allocated-fun (replace-vars-with-regs fun colors-hash)]
               [fn-name (if (label? (first allocated-fun))
                            (first allocated-fun)
                            #f)])
          (if fn-name
              (append (list fn-name)
                      (list 'esp '-= (* attempt 4))
                      (rest allocated-fun))
              (cons (list 'esp '-= (* attempt 4)) 
                    allocated-fun)))
          (L2f->L1f-internal (spill-function fun vars attempt)
                             (add1 attempt)
                             (rest vars)))))

(define/contract (spill-function fun vars attempt)
  (-> list? list? number? list?)
  (if (empty? vars)
      (display
       (format '"could not register allocate ~A'"
               (first fun)))
      (spill fun (first vars) (* (add1 attempt) -4)
             (format "_~Amagicyolovar" attempt))))
    

(define/contract (replace-vars-with-regs fun color-map)
  (-> list? hash? list?)
  (let ([result-fun fun])
    (for ([var (hash-keys color-map)])
      (set! result-fun 
            (replace-all-instrs var
                                (hash-ref color-map var)
                                result-fun)))
    result-fun))

(define (replace-all-instrs old new code)
  (for/list ([instr code])
    (replace-instr old new instr)))
         
(define (replace-instr old new instr)
  (cond [(empty? instr) '()]
        [(label? instr) instr]
        [(and (symbol? (car instr))
              (symbol=? (car instr) old))
         (cons new (replace-instr old new (rest instr)))]
        [(list? (car instr))
         (cons (replace-instr old new (car instr))
               (replace-instr old new (cdr instr)))]
        [else (cons (first instr)
                    (replace-instr old new (rest instr)))]))


(if (not (= (vector-length (current-command-line-arguments)) 1))
    (display "")
    (call-with-input-file
        (vector-ref (current-command-line-arguments) 0)
      (lambda (x) (display (L2->L1 (read x))))))
         

                 

(check-equal? (replace-instr 's 'eax '(s <- 5))
              '(eax <- 5))
(check-equal? (replace-instr 's 'ebx '((mem ebp -4) <- s))
              '((mem ebp -4) <- ebx))
(check-equal? (replace-instr 's 'ebx '(s <- (mem ebp -4)))
              '(ebx <- (mem ebp -4)))
(check-equal? (replace-instr 's 'ecx '(eax <- (print s)))
              '(eax <- (print ecx)))
(check-equal? (replace-instr 's 'edx '(eax <- (allocate s y)))
              '(eax <- (allocate edx y)))
(check-equal? (replace-instr 's 'edx '(cjump s < s :blah :bla))
              '(cjump edx < edx :blah :bla))
                             

              

(check-equal? (L2f->L1f '(:f (eax <- 1)))
              '(:f (eax <- 1)))
(check-equal? (L2f->L1f '(:f (eax <- 1) (eax += 2)))
              '(:f (eax <- 1) (eax += 2)))
(check-equal? (L2f->L1f '(:f (s0 <- 1)))
              '(:f (eax <- 1)))
(check-equal? (L2f->L1f '(:f (ebx <- 1) (s <- 1) (ebx += 5)))
              '(:f (ebx <- 1) (eax <- 1) (ebx += 5)))

(check-equal? (replace-instr 's 'ebx '((mem ebp -4) <- s))
              '((mem ebp -4) <- ebx))
(check-equal? (L2f->L1f '(:f (eax <- 1) (s <- 1) (eax += 5)))
              '(:f (eax <- 1) (ebx <- 1) (eax += 5)))
(check-equal? (L2f->L1f '(:f (eax <- 1) (s <- 2) (y += s) (eax += 5)))
              '(:f (eax <- 1) (ecx <- 2) (ebx += ecx) (eax += 5)))

(check-equal? (L2f->L1f '((abc <- 11) (abc <- 11) (tail-call s0)))
              '((esp -= 4)
                ((mem ebp -4) <- 11)
                ((mem ebp -4) <- 11)
                (tail-call ebx)))

(check-equal? (L2->L1 '(((eax <- 1) (x <- 5) (call :foo)) (:foo (ebx += 10))))
              '(((eax <- 1) (ebx <- 5) (call :foo)) (:foo (ebx += 10))))

#;; I think they register allocated poorly...this should be good
(check-equal? (L2->L1 '(((a <- 5)
                         (ecx <- a)
                         :loop
                         (cjump a <= 1 :end :cont)
                         :cont
                         (a -= 1)
                         (ecx *= a)
                         (goto :loop)
                         :end
                         (ecx *= 2)
                         (ecx += 1)
                         (eax <- (print ecx)))))
              '(((edx <- 5)
                 (ecx <- edx)
                 :loop
                 (cjump edx <= 1 :end :cont)
                 :cont
                 (edx -= 1)
                 (ecx *= edx)
                 (goto :loop)
                 :end
                 (ecx *= 2)
                 (ecx += 1)
                 (eax <- (print ecx)))))



              
              

