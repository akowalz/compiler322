#lang racket

;(require racket/include)
;(include "liveness.rkt")
(require rackunit
         "liveness.rkt")

; label? tests
(test-case
 "Label? Tests"
 (check-equal? (label? ':hello) #t)
 (check-equal? (label? 5) #f)
 (check-equal? (label? '_hello) #f)
 (check-equal? (label? '(a b 5)) #f))

(test-case
 "Preds Tests"
 (check-equal? (preds 0 '(:rrr (eax <- 3) (eax += 4) (ebx <- 4) (goto :rrr)))
               '(4))
 (check-equal? (preds 3 '(:f (eax <- 3) (eax += 4) :rrr (ebx <- 4) (goto :rrr)))
               '(5 2))
 (check-equal? (preds 4 '(:f (eax <- 3) (cjump 2 < 4 :f :rrr) (eax += 4) :rrr (ebx <- 4) (goto :rrr)))
               '(6 2 3))
 (check-equal? (preds 2 '(:f (return) (eax <- 1)))
               '())
 (check-equal? (preds 2 '(:f (call :g) (eax <- 1)))
               '())
 (check-equal? (preds 2 '(:f (tail-call :g) (eax += 1)))
               '())
 (check-equal? (preds 3 '(:f (cjump eax = eax :here :there) :here :there))
               '(1))
 (check-equal? (preds 2 '(:f (cjump eax = 2 :l1 :l2) :lx :l1 :l2))
               '())
 (check-equal? (preds 2 '(:f (cjump eax < 1 :l1 :l2) (eax <- 3) :l1 :l2))
               '())
 (check-equal? (preds 2 '(:f (goto :g) (eax <- 1) :g))
               '())
 
 (check-equal? (preds 3 '(:f (eax <- 1) (cjump 2<4 :f :f) (eax <- 4)))
               '())
 (check-equal? (preds 5 '(:f (return) (eax <- 1) (x <- eax) (eax <- 4) (eax += 1)))
               '())
 (check-equal? (preds 3 '(:f (call :fun) (eax <- 1) (eax <- 3)))
               '())
 (check-equal? (preds 4 '(:f (tail-call :fun) (goto :r) (eax <- 1) :r))
               '())
 (check-equal? (preds 3 '(:f (cjump eax < 5 :l1 :l2) (return) :l1 :l2))
               '(1))
 (check-equal? (preds 4 '(:f (cjump eax < 5 :l1 :l2) :l1 (return) :l2))
               '(1)))
 

(test-case
 "All-preds tests"
 (check-equal? (all-preds '(:f (eax <- 1)))
               '(() (0)))
 (check-equal? (all-preds '(:f (eax <- 1) (edx <- 2)))
               '(() (0) (1)))
 (check-equal? (all-preds '(:f (eax += 5) ((mem ebp -4) <- 11)))
               '(() (0) (1)))
 (check-equal? (all-preds '(:f (eax += 5) :g ((mem ebp -4) <- 11)))
               '(() (0) (1) (2)))
 (check-equal? (all-preds '(:f (eax += 5) (goto :g) :g))
               '(() (0) (1) (2)))
 (check-equal? (all-preds
                '(:f (cjump eax = eax :here :there) :here (eax <- 0) :there (eax <- 1)))
               '(() (0) (1) (2) (1 3) (4)))
 (check-equal? (all-preds '(:g (eax <- 1) (eax <- (print eax)) (x <- 5)))
               '(() (0) (1) (2)))
 (check-equal? (all-preds '(:f (x <- 1) (y <- 2) (edx <- x = y)))
               '(() (0) (1) (2)))
 ; this is a really tough test case
 ; :h is actually unreachable, becuase the goto will always skip it
 ; so, if :h is unreachable, :g cannot be reached through h
 ; to fix: maybe add a recursive call on on each node to check? could get expensive
 (check-equal? (all-preds 
                '(:f (eax += 5) (goto :g) :h :g))
                '(() (0)        (1)       () (2)))
 ; also fails, because (x <- 5) can't be reached
 (check-equal? (all-preds '(:f (cjump eax < eax :l1 :l2) (x <- 5) :l1 :l2))
               '(() (0) () (1) (1 3)))
 (check-equal? (all-preds '(:g (goto :r) :q :r))
               '(() (0) () (1)))
 ;a also tough we never configured calls either
 (check-equal? (all-preds '(:g (call :h) (eax += 1)))
               '(() (0) ()))
 (check-equal? (all-preds '(:h (return) :g))
               '(() (0) ()))
 (check-equal? (all-preds '(:g (tail-call :h) (x <- 10)))
               '(() (0) ()))
 (check-equal? (all-preds '(:g (eax <- :fun) (tail-call :fun) (eax <- 1)))
               '(() (0) (1) ())))

(test-case
 "copy not killed tests"
 (check-equal? (copy-not-killed '(() (a) (b)) '((a) (b) ()) '((a) () ()))
               '(() (a b) (b)))
 (check-equal? (copy-not-killed '(() (eax edx x y) (z))
                                '((ebx eax) (eax edx x y z) ())
                                '((ebx) () ()))
               '((eax) (y x edx eax z) (z))))

;make list of empties test
(check-equal? (make-list-of-empties 3)
              '(() () ()))
(test-case
 "get new outs"
 (check-equal? (get-new-outs '((a) (b)) '(0 1) '())
               '(b a))
 (check-equal? (get-new-outs '((a) (b)) '(0 1) '(c))
               '(b c a))
 (check-equal? (get-new-outs '((a) (b)) '(0) '(c))
               '(c a))
)


(test-case
 "copy inds"
; hold on, I think this is correct...
; no one is referencing slot 2, so eax is never being copied. 
; Is that wrong? We had thought this was a bug
 (check-equal? (copy-inds '(() () (eax)) '(() (0) (1)) '(() () ()))
               '(() () ())) ; on Saturday we were expecting '(() (eax) ())
 
 
 (check-equal? (copy-inds '(() (a)) '(() (1)) '(() ()))
               '(() (a)))
 (check-equal? (copy-inds '((a)) '((0)) '(()))
               '((a)))
 (check-equal? (copy-inds '((a) (a) (a)) '(() () (0 1 2)) '(() () ()))
               '(() () (a)))
  
 (check-equal? (copy-inds '((a) (b) (c)) '((2 1) (1) (0)) '(() () ()))
               '((b c) (b) (a))) 
 (check-equal? (copy-inds '(() (eax) (x eax))
                          '((0) (2 1) (1))
                          '((edx) () ()))
               '((edx) (x eax) (eax))))

; kill and gen
(test-case
 "Kills and gens"
 (check-equal? (all-gens '(:f (eax <- 4) (eax += 1)))
               '(() ()(eax)))
 (check-equal? (all-kills '(:f (eax <- 4) (eax += 1)))
               '(() (eax) (eax)))
 (check-equal? (all-kills '(:f (eax <- 10) (eax += 10) (cjump eax = eax :f :f)))
               '(() (eax) (eax) ()))
 (check-equal? (all-gens '(:f (eax <- 10) (eax += 10) (cjump eax = eax :f :f)))
               '(() () (eax) (eax)))
 (check-equal? (all-gens '(:f (x <- 10) (y <- 11) (x >>= y) (cjump x < y :f :f)))
               '(() () () (x y) (x y)))
 (check-equal? (all-kills '(:f (x <- 10) (y <- 11) (x >>= y) (cjump x < y :f :f)))
               '(() (x) (y) (x) ()))
 (let ([prog '(:f (x <- 10) (x <- (mem ebp -4)) ((mem ebp -8) <- x))])
   (check-equal? (all-kills prog)
                 '(() (x) (x) ()))
   (check-equal? (all-gens prog)
                 '(() () (ebp) (ebp x))))
 (let ([prog '(:f (eax <- 11) (eax <- (print eax)))])
   (check-equal? (all-kills prog)
                 '(() (eax) (eax ecx edx)))
   (check-equal? (all-gens prog)
                 '(() () (eax))))
 (let ([prog '(:f (z <- 11) (y <- (mem ebp -4)) (eax <- (allocate z y)))])
   (check-equal? (all-kills prog)
                 '(() (z) (y) (eax ecx edx)))
   (check-equal? (all-gens prog)
                 '(() () (ebp) (z y))))
 (let ([prog '(:f (a <- 5) (b <- 7) (eax <- (array-error a b)))])
   (check-equal? (all-kills prog)
                 '(() (a) (b) (eax ecx edx)))
   (check-equal? (all-gens prog)
                 '(() () () (a b))))
 (let ([prog '(:f (x <- 11) (eax <- (array-error x x)))])
   (check-equal? (all-kills prog)
                 '(() (x) (eax ecx edx)))
   (check-equal? (all-gens prog)
                 '(() () (x))))
 (let ([prog '(:f (call :g))])
   (check-equal? (all-kills prog)
                 '(() (eax ebx ecx edx)))
   (check-equal? (all-gens prog)
                 '(() (eax ecx edx))))
 (let ([prog '(:f (y <- :g) (call y))])
   (check-equal? (all-kills prog)
                 '(() (y) (eax ebx ecx edx)))
   (check-equal? (all-gens prog)
                 '(() () (y eax ecx edx))))
 (let ([prog '(:f (tail-call :g))])
   (check-equal? (all-kills prog)
                 '(() ()))
   (check-equal? (all-gens prog)
                 '(() (eax ecx edx esi edi))))
 (let ([prog '(:f (a <- :g) (tail-call a))])
   (check-equal? (all-kills prog)
                 '(() (a) ()))
   (check-equal? (all-gens prog)
                 '(() () (a eax ecx edx esi edi))))
 (let ([prog '(:f (return))])
   (check-equal? (all-kills prog)
                 '(() ()))
   (check-equal? (all-gens prog)
                 '(() (eax esi edi))))
 (let ([prog '(:f (goto :r) :r (x <- 10))])
   (check-equal? (all-kills prog)
                 '(() () () (x)))
   (check-equal? (all-gens prog)
                 '(() () () ())))
 (let ([prog '(:f (x <- 11) (y <- x) (edx <- x = y))])
   (check-equal? (all-kills prog)
                 '(() (x) (y) (edx)))
   (check-equal? (all-gens prog)
                 '(() () (x) (x y))))
 (let ([prog '(:f (x <- 11) (z <- x <= x))])
   (check-equal? (all-kills prog)
                 '(() (x) (z)))
   (check-equal? (all-gens prog)
                 '(() () (x)))))


(test-case 
 "Find refs"
 (check-equal? (find-refs ':hello '(:f (cjump 1 = 1 :hello :f) (goto :hello) :hello))
               '(2 1))
 (check-equal? (find-refs ':g '(:f (goto :g) :g))
               '(1))
 ;here's a tough one.  Maybe we should implement a "reacheable?" function
 ;Not exactly sure what we should return...there is a reference after all
 (check-equal? (find-refs ':h '(:f (return) (cjump 1 = 1 :h :j) :h))
               '())
 (check-equal? (find-refs ':out-of-reach '(:f (cjump 1 = 1 :in-reach :in-reach)
                                               :out-of-reach :in-reach))
               '())
 (check-equal? (find-refs ':no '(:f (goto :yes) :no :yes))
               '())
 (check-equal? (find-refs ':yes '(:f (goto :yes) :yes))
               '(1))

