#lang racket
(require rackunit racket/set)
;L3 compiler

(define bounds-count 0)
(define if-counter 0)
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
   (compile-e-int e))

(define (compile-e-int e)
  (match e 
    [`(let ([,x ,d]) ,e1) (compile-let x d e1)]
    [`(if ,test ,then ,else) (compile-if test then else)]
    [d (let ([d-code (compile-d d 'eax #t)])
                      (if (list-contains? 'tail-call d-code)
                          d-code
                          (append d-code `((return)))))]))
  

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
    [`(+ ,v1 ,v2) (compile-add/sub dest v1 v2 '+=)]
    [`(- ,v1 ,v2) (compile-add/sub dest v1 v2 '-=)]
    [`(* ,v1 ,v2) `((yoloswaggins <- ,(encode v1))
                    (yoloswaggins >>= 1)
                    (,dest <- ,(encode v2))
                    (,dest >>= 1)
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
    [v `((,dest <- ,(encode v)))]));check for labels

(define (tmp)
    (string->symbol
     (string-append "__x_"
                    (number->string (random 1000)))))

(define (compile-add/sub dest v1 v2 op)
  (let* ([temp (tmp)]
         (store-instr `(,temp <- ,dest))
         (not-op (if (symbol=? op '+=)
                     '-=
                     '+=))
         (newv1  (encode v1))
         (newv2 (if (and (not (number? v2)) (symbol=? v2 dest))
                    temp
                    (encode v2))))
    (cond [(equal? v1 dest)
           `((,dest ,op ,newv2)
             (,dest ,not-op 1))]
          [(equal? temp newv2)
           `(,store-instr
             (,dest <- ,newv1)
             (,dest ,op ,newv2)
             (,dest ,not-op 1))]
          [else 
           `((,dest <- ,newv1)
             (,dest ,op ,newv2)
             (,dest ,not-op 1))])))

(define (compile-call fn arglist tail? dest)
  (append (for/list [(arg arglist)
                     (i (in-naturals))]
            `(,(list-ref arg-registers i) <- ,(encode arg)))
          (if tail?
              `((tail-call ,fn))
              `((call ,fn) (,dest <- eax)))))
           

(define (compile-comp op v1 v2 dest)
 `((,dest <- ,(encode v1) ,op ,(encode v2))
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
        [fail (new-bounds-label bounds-count "fail")]
        [bounds-temp (tmp)])
    (set! bounds-count (add1 bounds-count))
    `((,dest <- ,(encode pos))
      (,dest >>= 1)
      (,bounds-temp <- (mem ,array 0))
      (cjump ,dest < ,bounds-temp ,pass ,fail)
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

(define (compile-let dest dexpr e)
  (let* ((newIC (compile-e-int e))
         (instrs newIC))
     (append (compile-d dexpr dest #f) 
             instrs)))


(define (compile-if test then else)
  (set! if-counter (+ 1 if-counter))
  (let* ([else-lab (new-if-label "else")]
         [then-lab (new-if-label "then")]
         [compiled-then (compile-e-int then)]
         [compiled-else (compile-e-int else)])
  (append `((cjump ,(encode test) = 1 ,else-lab ,then-lab))
                         (list then-lab)
                         compiled-then
                         (list else-lab)
                         compiled-else)
                 ))


(define (new-if-label then/else)
  (string->symbol
   (string-append ":magic_if_label_"
                  (number->string if-counter)
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


#|
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
                (x >>= 1)
                (x *= yoloswaggins)
                (x *= 2)
                (x += 1)
                (eax <- x)
                (return)))

(check-equal? (compile-e '(let ([x (* 10 11)]) (let ([y 3]) (+ y x))))
              '((yoloswaggins <- 21) 
                (yoloswaggins >>= 1) 
                (x <- 23)
                (x >>= 1)
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

(check-equal? (L3->L2 '(
  (let ([result (* 1 0)])
    (if result (:truefunc) (:falsefunc))
  )
  (:truefunc () (print 100))
  (:falsefunc () (print 200))
)
) '())
|#
#;
(check-equal? (L3->L2 '((let ((a -1)) (let ((a (- 5 a))) (print a)))))
              '(((call :main123))
                (:main123
                 (a <- -1)
                 (x_1 <- a)
                 (a <- 11)
                 (a -= x_1)
                 (a += 1)
                 (eax <- (print a))
                 (return))))
#;
(check-equal? (L3->L2 '((let ((a -1)) (let ((a (- a 5))) (print a)))))
              '(((call :main123))
                (:main123
                 (a <- -1)
                 (x_1 <- a)
                 (a <- x_1)
                 (a -= 11)
                 (a += 1)
                 (eax <- (print a))
                 (return))))
#;
(L3->L2 '((let ((x_2 (:fib 5))) (print x_2))
 (:fib
  (n_1)
  (let ((x_3 (< n_1 2)))
    (if x_3
      1
      (let ((x_4 (- n_1 1)))
        (let ((x_5 (:fib x_4)))
          (let ((x_6 (- n_1 2))) (let ((x_7 (:fib x_6))) (+ x_5 x_7)))))))))
)

(check-equal? (compile-e `(if 1 2 3))
              `((cjump 3 = 1 :magic_if_label_1else :magic_if_label_1then)
                :magic_if_label_1then
                (eax <- 5)
                (return)
                :magic_if_label_1else
                (eax <- 7)
                (return)))

(check-equal? (compile-e `(if x (if 0 4 5) 3))
              `((cjump x = 1 :magic_if_label_2else :magic_if_label_2then)
                :magic_if_label_2then
                (cjump 1 = 1 :magic_if_label_3else :magic_if_label_3then)
                :magic_if_label_3then
                (eax <- 9)
                (return)
                :magic_if_label_3else
                (eax <- 11)
                (return)
                :magic_if_label_2else
                (eax <- 7)
                (return)))


;(L3->L2 '((let ((a -1)) (let ((a (+ a 5))) (print a)))))
;(L3->L2 '((let ((a 3)) (let ((a (+ a a))) (print a)))))
#;
(L3->L2 '((let ((a 1)) (let ((a (new-tuple a 2 3))) (aref a a)))))


(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (λ (x) (display (L3->L2 (read x))))))

(provide L3->L2)
              
                     
              
                     
               
    