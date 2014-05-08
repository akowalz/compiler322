#lang racket
(require rackunit)
;L3 compiler

(struct instr/count (instr count))

(define (compile-e e)
  (instr/count-instr
   (compile-e-int e 0)))

(define (compile-e-int e count)
  (match e 
    [`(let ([,x ,d]) ,e1) (compile-let x d e1 count)]
    [`(if ,test ,then ,else) (compile-if test then else count)]
    [d (instr/count (compile-d d 'eax) count)]))
  

(define (compile-d d dest)
  (match d
    [`(aref ,a ,off) 'stub]
    [`(aset ,a ,off ,new) 'stub]
    [`(alen ,a) 'stub]
    [`(new-array ,size ,init) 'stub]
    [`(new-tuple ,vs ...) 'stub]
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
    [`(number? ,v) (compile-huh? 'number? v dest)]
    [`(a? ,v) (compile-huh? 'a? v dest)]
    [`(print ,v) `((eax <- (print ,v)))]
    [`(make-closure ,label ,v) (compile-d `(new-tuple ,label ,v))]
    [`(closure-proc ,v) (compile-d `(aref ,v 0) dest)]
    [`(closure-vars ,v) (compile-d `(aref ,v 1) dest)]
    [`(,fn) 'stub]
    [`(,fn ,v1) 'stub]
    [`(,fn ,v1 ,v2) 'stub]
    [`(,fn ,v1 ,v2 ,v3) 'stub]
    [v `((,dest <- ,(encode v)))] ;check for labels
    ))

(define (compile-comp op v1 v2 dest)
 `((,dest <- ,v1 ,op ,v2)
   (,dest <<= 1)
   (,dest += 1)))

(define (compile-huh? huh v1 dest)
  (append `((,dest <- ,(encode v1))
            (,dest &= 1))
          (cond [(symbol=? huh 'a?)
                 `((,dest *= -2)
                   (,dest += 3))]
                [else 
                 `((,dest *= 2)
                   (,dest += 1))])))
                
                  
                  

(define (compile-let dest dexpr e count)
  (let* ((newIC (compile-e-int e count))
         (instrs (instr/count-instr newIC))
         (new-count (instr/count-count newIC)))
  (instr/count
   (append (compile-d dexpr dest) 
           instrs)
   new-count)))


(define (compile-if test then else count)
  (let* ([else-lab (new-if-label count "else")]
         [then-lab (new-if-label count "then")]
         [compiled-then (compile-e-int then (+ 1 count))]
         [compiled-else (compile-e-int else 
                                       (instr/count-count compiled-then))])
    (instr/count (append `((cjump ,test = 0 ,else-lab ,then-lab))
                         (list then-lab)
                         (instr/count-instr compiled-then)
                         (list else-lab)
                         (instr/count-instr compiled-else))
                 (instr/count-count compiled-else))
    ))


(define (new-if-label count then/else)
  (string->symbol
   (string-append ":magic-if-label-"
                  (number->string count)
                  then/else)))
  
  

(define/contract (encode x)
  (-> (or/c number? symbol?)
      (or/c number? symbol?))
  (cond 
    ((number? x) (+ 1 (* 2 x)))
    ((symbol? x) x)))

(check-equal? (compile-e '(let ([x 5]) x))
              '((x <- 11)
                (eax <- x)))
(check-equal? (compile-e '(let ([x (+ 2 3)]) x))
              '((x <- 5)
                (x += 7)
                (x -= 1)
                (eax <- x)))

(check-equal? (compile-e '(let ([x (- 3 2)]) x))
              '((x <- 7)
                (x -= 5)
                (x += 1)
                (eax <- x)))

(check-equal? (compile-e '(let ([x (* 10 11)]) x))
              '((yoloswaggins <- 21)
                (yoloswaggins >>= 1)
                (x <- 23)
                (x *= yoloswaggins)
                (x *= 2)
                (x += 1)
                (eax <- x))
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
                (eax -= 1))
              )

(check-equal? (compile-e `(let ([x (< 4 5)]) (+ x 3))) 
              '((x <- 4 < 5)
                (x <<= 1)
                (x += 1)
                (eax <- x)
                (eax += 7)
                (eax -= 1)))
(check-equal? (compile-e `(a? 3)) 
              '((eax <- 7)
                (eax &= 1)
                (eax *= -2)
                (eax += 3)))
(check-equal? (compile-e `(number? 3)) 
              '((eax <- 7)
                (eax &= 1)
                (eax *= 2)
                (eax += 1)))

(check-equal? (compile-e `(if 1 2 3))
              `((cjump 1 = 0 :magic-if-label-0else :magic-if-label-0then)
                :magic-if-label-0then
                (eax <- 5)
                :magic-if-label-0else
                (eax <- 7)))


              
               
    