#lang racket
(require rackunit racket/set)
;L3 compiler

(struct instr/count (instr count))
(define bounds-count 0)
(define arg-registers '(ecx edx eax))

(define (L3->L2 prog)
  (append (list '((call :main123)))
          (list (cons ':main123 
                      (compile-e (first prog))))
          (map compile-L3f (rest prog))))

(define (compile-L3f fn)
  (let ([name (first fn)]
        [arglist (second fn)]
        [e (third fn)])
    (append (list name)
            (for/list ((arg arglist)
                       (i (in-naturals)))
              `(,arg <- ,(list-ref arg-registers i)))
            (compile-e e))))

(define/contract (list-contains? sym lst)
  (-> symbol? list? boolean?)
  (cond [(empty? lst) #f]
        [(list? (car lst)) (or (list-contains? sym (car lst))
                               (list-contains? sym (cdr lst)))]
        [(symbol? (car lst)) (or (symbol=? (car lst) sym)
                                 (list-contains? sym (cdr lst)))]
        [else #f]))

(define (compile-e e)
  (instr/count-instr
   (compile-e-int e 0)))

(define (compile-e-int e count)
  (match e 
    [`(let ([,x ,d]) ,e1) (compile-let x d e1 count)]
    [`(if ,test ,then ,else) (compile-if test then else count)]
    [d (instr/count (let ([d-code (compile-d d 'eax #t)])
                      (if (list-contains? 'tail-call d-code)
                          d-code
                          (append d-code `((return))))) count)]))
  

(define (compile-d d dest tail?)
  (match d
    [`(aref ,a ,off) (aref a off dest) ]
    [`(aset ,a ,off ,new) (aset a off new dest) ]
    [`(alen ,a) `((,dest <- (mem ,a 0))
                  (,dest <<= 1)
                  (,dest += 1))]
    [`(new-array ,size ,init) `((eax <- (allocate ,(encode size) ,(encode init)))
                                (,dest <- eax))]
    [`(new-tuple ,vs ...) (init-tuple vs dest)]
    ;biops
    [`(+ ,v1 ,v2) `((,dest <- ,(encode v1))
                    (,dest += ,(encode v2))
                    (,dest -= 1))]
    [`(- ,v1 ,v2) `((,dest <- ,(encode v1))
                    (,dest -= ,(encode v2))
                    (,dest += 1))]
    [`(* ,v1 ,v2) `((yoloswaggins <- ,(encode v1))
                    (yoloswaggins >>= 1)
                    (,dest <- ,(encode v2))
                    (,dest *= yoloswaggins)
                    (,dest *= 2)
                    (,dest += 1))]
    [`(< ,v1 ,v2) (compile-comp '< v1 v2 dest)]
    [`(<= ,v1 ,v2) (compile-comp '<= v1 v2 dest)]
    [`(= ,v1 ,v2) (compile-comp '= v1 v2 dest)]
    ;preds
    [`(number? ,v) (compile-huh 'number? v dest)]
    [`(a? ,v) (compile-huh 'a? v dest)]
    [`(print ,v) `((eax <- (print ,(encode v))))]
    [`(make-closure ,label ,v) (compile-d `(new-tuple ,label ,v) dest tail?)]
    [`(closure-proc ,v) (compile-d `(aref ,v 0) dest tail?)]
    [`(closure-vars ,v) (compile-d `(aref ,v 1) dest tail?)]
    [`(,fn) (compile-call fn '() tail? dest) ]
    [`(,fn ,arglist ...) (compile-call fn arglist tail? dest)]
    [v `((,dest <- ,(encode v)))])) ;check for labels

(define (compile-call fn arglist tail? dest)
  (append (for/list [(arg arglist)
                     (i (in-naturals))]
            `(,(list-ref arg-registers i) <- ,(encode arg)))
          (if tail?
              `((tail-call ,fn))
              `((call ,fn) (,dest <- eax)))))
           

(define (compile-comp op v1 v2 dest)
 `((,dest <- ,v1 ,op ,v2)
   (,dest <<= 1)
   (,dest += 1)))

(define (compile-huh huh v1 dest)
  (append `((,dest <- ,(encode v1))
            (,dest &= 1))
          (cond [(symbol=? huh 'a?)
                 `((,dest *= -2)
                   (,dest += 3))]
                [else 
                 `((,dest *= 2)
                   (,dest += 1))])))
                
(define (aset array pos new dest)
  (let ([pass (new-bounds-label bounds-count "pass")]
        [fail (new-bounds-label bounds-count "fail")])
    (set! bounds-count (add1 bounds-count))
    `((,dest <- ,(encode pos))
      (,dest >>= 1)
      (bounds-temp <- (mem ,array 0))
      (cjump ,dest < bounds-temp ,pass ,fail)
      ,fail
      (eax <- (array-error ,array ,(encode pos)))
      ,pass
      (,dest *= 4)
      (,dest += ,array)
      ((mem ,dest 4) <- ,(encode new))
      (,dest <- 1))))

(define (aref array pos dest)
  (let ([pass (new-bounds-label bounds-count "pass")]
        [fail (new-bounds-label bounds-count "fail")])
    (set! bounds-count (add1 bounds-count))
    `((,dest <- ,(encode pos))
      (,dest >>= 1)
      (bounds-temp <- (mem ,array 0))
      (cjump ,dest < bounds-temp ,pass ,fail)
      ,fail
      (eax <- (array-error ,array ,(encode pos)))
      ,pass
      (,dest *= 4)
      (,dest += ,array)
      (,dest <- (mem ,dest 4)))))                  

(define (compile-let dest dexpr e count)
  (let* ((newIC (compile-e-int e count))
         (instrs (instr/count-instr newIC))
         (new-count (instr/count-count newIC)))
    (instr/count
     (append (compile-d dexpr dest #f) 
             instrs)
     new-count)))


(define (compile-if test then else count)
  (let* ([else-lab (new-if-label count "else")]
         [then-lab (new-if-label count "then")]
         [compiled-then (compile-e-int then (+ 1 count))]
         [compiled-else (compile-e-int else (instr/count-count compiled-then))])
    (instr/count (append `((cjump ,test = 0 ,else-lab ,then-lab))
                         (list then-lab)
                         (instr/count-instr compiled-then)
                         (list else-lab)
                         (instr/count-instr compiled-else))
                 (instr/count-count compiled-else))))


(define (new-if-label count then/else)
  (string->symbol
   (string-append ":magic_if_label_"
                  (number->string count)
                  then/else)))

(define (new-bounds-label count pass/fail)
  (string->symbol
   (string-append ":magic_bounds_label_"
                  (number->string count)
                  pass/fail)))


(define (init-tuple values dest)
  (append `((eax <- (allocate ,(encode (length values)) 0)))
          (for/list ((v values)
                     (i (in-naturals)))
          `((mem eax ,(* 4 (+ 1 i))) <- ,(encode v)))
          `((,dest <- eax)))) 

(define/contract (encode x)
  (-> (or/c number? symbol?)
      (or/c number? symbol?))
  (cond 
    ((number? x) (+ 1 (* 2 x)))
    ((symbol? x) x)))

(check-equal? (init-tuple '(3 4 5) 'x)
              `((eax <- (allocate 7 0))
                ((mem eax 4) <- 7)
                ((mem eax 8) <- 9)  
                ((mem eax 12) <- 11)
                (x <- eax)))  

(check-equal? (compile-e '(let ([x 5]) x))
              '((x <- 11)
                (eax <- x)
                (return)))

(check-equal? (compile-e '(let ([x (+ 2 3)]) x))
              '((x <- 5)
                (x += 7)
                (x -= 1)
                (eax <- x)
                (return)))

(check-equal? (compile-e '(let ([x (- 3 2)]) x))
              '((x <- 7)
                (x -= 5)
                (x += 1)
                (eax <- x)
                (return)))

(check-equal? (compile-e '(let ([x (* 10 11)]) x))
              '((yoloswaggins <- 21)
                (yoloswaggins >>= 1)
                (x <- 23)
                (x *= yoloswaggins)
                (x *= 2)
                (x += 1)
                (eax <- x)
                (return))
              )

(check-equal? (compile-e '(let ([x (* 10 11)]) (let ([y 3]) (+ y x))))
              '((yoloswaggins <- 21) 
                (yoloswaggins >>= 1) 
                (x <- 23)
                (x *= yoloswaggins) 
                (x *= 2) 
                (x += 1) 
                (y <- 7) 
                (eax <- y) 
                (eax += x) 
                (eax -= 1)
                (return))
              )

(check-equal? (compile-e `(let ([x (< 4 5)]) (+ x 3))) 
              '((x <- 4 < 5)
                (x <<= 1)
                (x += 1)
                (eax <- x)
                (eax += 7)
                (eax -= 1)
                (return)))
(check-equal? (compile-e `(a? 3)) 
              '((eax <- 7)
                (eax &= 1)
                (eax *= -2)
                (eax += 3)
                (return)))
(check-equal? (compile-e `(number? 3)) 
              '((eax <- 7)
                (eax &= 1)
                (eax *= 2)
                (eax += 1)
                (return)))

(check-equal? (compile-e `(if 1 2 3))
              `((cjump 1 = 0 :magic_if_label_0else :magic_if_label_0then)
                :magic_if_label_0then
                (eax <- 5)
                (return)
                :magic_if_label_0else
                (eax <- 7)
                (return)))

(check-equal? (compile-e `(if x (if 0 4 5) 3))
              `((cjump x = 0 :magic_if_label_0else :magic_if_label_0then)
                :magic_if_label_0then
                (cjump 0 = 0 :magic_if_label_1else :magic_if_label_1then)
                :magic_if_label_1then
                (eax <- 9)
                (return)
                :magic_if_label_1else
                (eax <- 11)
                (return)
                :magic_if_label_0else
                (eax <- 7)
                (return)))
(check-equal? (compile-e `(f)) 
              `((tail-call f)))
(check-equal? (compile-e `(f y)) 
              `((ecx <- y)
                (tail-call f)))

(check-equal? (compile-e `(let ([x (f y)]) x)) 
              `((ecx <- y)
                (call f)
                (x <- eax)
                (eax <- x)
                (return)))
              
(check-equal? (compile-e `(let ([x (f z z1 z2)]) x)) 
              `((ecx <- z)
                (edx <- z1)
                (eax <- z2)
                (call f)
                (x <- eax)
                (eax <- x)
                (return)))

(check-equal? (L3->L2 `((let ([x 5]) x)))
              '(((call :main123))
                (:main123
                 (x <- 11)
                 (eax <- x)
                 (return))))

(check-equal? (L3->L2 `((let ([x 5]) (print x))))
              '(((call :main123))
               (:main123 (x <- 11) (eax <- (print x)) (return))))
(check-equal? (L3->L2 '((let ([x (+ 1 4)]) (print x)))) 
              '(((call :main123)) (:main123 (x <- 3) (x += 9) (x -= 1)
                                            (eax <- (print x)) (return))))
(check-equal? (L3->L2 '((let ([x (:two_plus_two)]) (print x))
                        (:two_plus_two () (+ 2 2)))) 
              '(((call :main123))
                (:main123
                 (call :two_plus_two)
                 (x <- eax)
                 (eax <- (print x))
                 (return))
                (:two_plus_two
                 (eax <- 5)
                 (eax += 5)
                 (eax -= 1)
                 (return))))
(check-equal? (L3->L2 '((let ([x (:two_plus_z 3)]) (print x))
                        (:two_plus_z (z) (+ z 2)))) 
              '(((call :main123))
                (:main123
                 (ecx <- 7)
                 (call :two_plus_z)
                 (x <- eax)
                 (eax <- (print x))
                 (return))
                (:two_plus_z
                 (z <- ecx)
                 (eax <- z)
                 (eax += 5)
                 (eax -= 1)
                 (return))))

(check-equal? (L3->L2 '((let ([x (new-array 10 3)])
                          (let ([y (aref x 4)])
                            (print y))))) '(((call :main123))
                                            (:main123
                                             (eax <- (allocate 21 7))
                                             (x <- eax)
                                             (y <- 9)
                                             (y >>= 1)
                                             (bounds-temp <- (mem x 0))
                                             (cjump
                                              y
                                              <
                                              bounds-temp
                                              :magic_bounds_label_0pass
                                              :magic_bounds_label_0fail)
                                             :magic_bounds_label_0fail
                                             (eax <- (array-error x 9))
                                             :magic_bounds_label_0pass
                                             (y *= 4)
                                             (y += x)
                                             (y <- (mem y 4))
                                             (eax <- (print y))
                                             (return))))

(if (= (vector-length (current-command-line-arguments)) 1)
    (call-with-input-file
        (vector-ref (current-command-line-arguments) 0)
      (λ (x) (display (L3->L2 (read x)))))
    (display ""))
              
                     
              
                     
               
    