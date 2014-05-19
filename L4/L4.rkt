#lang plai
(print-only-errors)
#;
(define-type L4-e
  [let-4 (x var?)
         (n L4-e?)
         (b L4-e?)]
  [if-4 (test L4-e?)
        (then L4-e?)
        (else L4-e?)]
  [app-4 (fun L4-e?)
         (arg L4-e?)]
  [val-4 (v val?)])

(define (L4-e? e)
  true)

(define (val? v)
  (or (symbol? v) (number? v)))

(define (var? x)
  (symbol? x))

(define-type context
  [let-ctxt (x var?)
            (b L4-e?)
            (k context?)]
  [if-ctxt (t L4-e?)
           (e L4-e?)
           (k context?)]
  [fun-ctxt (a L4-e?)
            (k context?)]
  [arg-ctxt (f val?)
            (e L4-e?)
            (u L4-e?)
            (k context?)]
  
  [no-ctxt])

(define L4->L3
  (λ (prog)
    (cons (norm (first prog))
          (map (λ (fun)
                 (list (first fun) (second fun) (norm (third fun))))
               (rest prog)))))

(define (norm e)
  (define var-count 0)
  (define (fresh-var)
    (let ((v (string->symbol
              (string-append "__x"
                             (number->string var-count)))))
      (begin (set! var-count (add1 var-count))
             v)))
  (define (find e k)
    ;(-> L4-e? context? list?)
    (match e
      [`(begin ,e1 ,e2) (find `(let ([,(fresh-var) ,e1]) ,e2) k)]
      [`(let ([,x ,r]) ,b) (find r (let-ctxt x b k))]
      [`(if ,c ,t ,e) (find c (if-ctxt t e k))]
      [`(,f ,a ...) (find f (fun-ctxt a k))]
      [(? val?) (fill e k)]
      ))
  
  (define (fill d k)
    ;(-> L4-e? context? L3-e?)
    (type-case context k
      [let-ctxt (x b k1) 
                `(let ([,x ,d])
                   ,(find b k1))]
      [if-ctxt (t e k1) 
               (if (val? d)
                   `(if ,d
                        ,(find t k1)
                        ,(find e k1))
                   (let ([v (fresh-var)])
                     `(let ([,v ,d])
                        (if ,v
                            ,(find t k1)
                            ,(find e k1)))))]
      [fun-ctxt (a k1) 
                (if (empty? a)
                    (if (val? d) 
                        (fill `(,d) k1)
                        (let ([v (fresh-var)])
                          (fill `(,v) k1)))
                    (if (val? d)
                        (find (first a) (arg-ctxt d '() (rest a) k1))
                        (let ([v (fresh-var)])
                          `(let ([,v ,d])
                             ,(find (first a) (arg-ctxt v '() (rest a) k1))))))]
      [arg-ctxt (f e u k1) 
                (if (empty? u)
                    (if (val? d)
                        (fill `(,f ,@e ,d) k1)
                        (let ([v (fresh-var)])
                          `(let ([,v ,d])
                             ,(fill `(,f ,@e ,v) k1))))
                    (if (val? d)
                        (find (first u) (arg-ctxt f (append e (list d)) (rest u) k1))
                        (let ([v (fresh-var)])
                          `(let ([,v ,d])
                             ,(find (first u) (arg-ctxt f (append e (list v)) (rest u) k1))))))]
      [no-ctxt () d]
      ))
  
  (find e (no-ctxt)))



(test (norm `(let ([x 5])
               (a x))) 
      '(let ([x 5])
         (a x)))
(test (norm `(if 0 2 3))
      `(if 0 2 3))
(test (norm `(a (if 0 4 5)))
      `(if 0 (a 4) (a 5)))

(test (norm `(let ([x 5])
                (+ 5 (+ x 5)))) 
      `(let ([x 5])
          (let ([__x0 (+ x 5)])
            (+ 5 __x0))))
(test (norm `(begin 5 (x 3)))
      `(let ([__x0 5])
        (x 3)))

(test (norm `(begin (+ 5 11) (x 3 (b 2))))
      `(let ([__x0 (+ 5 11)])
         (let ([__x1 (b 2)])
           (x 3 __x1))))

(test (norm `(a (b c)))
      '(let ((__x0 (b c))) (a __x0)))

(test (norm '((a b) (b c)))
      '(let ((__x0 (a b)))
        (let ((__x1 (b c)))
          (__x0 __x1))))

(test (norm `(a (b c) (d e)))
      `(let ((__x0 (b c)))
         (let ((__x1 (d e)))
           (a __x0 __x1))))

(test (norm `(a (b c) (d (e f))))
      `(let ((__x0 (b c)))
         (let ((__x1 (e f)))
           (let ((__x2 (d __x1)))
             (a __x0 __x2)))))
         
(test (norm `(a (b c d)))
      '(let ((__x0 (b c d))) (a __x0)))

(test (norm `(let ((a (new-tuple 1 2)))
               (if
                (= (aref a 0)
                   (aref a 1))
                10
                20))) 
      '(let ((a (new-tuple 1 2)))
         (let ((__x0 (aref a 0)))
           (let ((__x1 (aref a 1)))
             (let ((__x2 (= __x0 __x1))) (if __x2 10 20))))))

(test (norm `(let ((a (new-tuple 0 0 0)))
            (begin
              (aset a 0 10)
              (print (aref a 0)))))
      `(let ((a (new-tuple 0 0 0)))
        (let ((__x0 (aset a 0 10)))
          (let ((__x1 (aref a 0)))
            (print __x1)))))
(test (norm '(print
              (= (:fun
                  (new-array 10 0) 101)
                 1)))
      `(let ((__x0 (new-array 10 0)))
         (let ((__x1 (:fun __x0 101)))
           (let ((__x2 (= __x1 1)))
             (print __x2)))))

(test (L4->L3 
       '((print (= (:len_greater_than_n
                    (new-array 10 0) 101)
                   1))
         (:len_greater_than_n (a n)
                              (<= n (alen a)))))
      '((let ((__x0 (new-array 10 0)))
    (let ((__x1 (:len_greater_than_n __x0 101)))
      (let ((__x2 (= __x1 1))) (print __x2))))
  (:len_greater_than_n
   (a n)
   (let ((__x0 (alen a))) (<= n __x0)))))

(if (= (vector-length (current-command-line-arguments)) 1)
    (call-with-input-file
        (vector-ref (current-command-line-arguments) 0)
                 (λ (x) (display (L4->L3 (read x)))))
    (display ""))
      
      
