#lang racket
(require "../hw1/compiler-L1.rkt"
         "../L2/L2.rkt"
         "../L3/L3.rkt"
         "../L4/L4.rkt"
         "../L5/L5.rkt")


(define (compile-L L5-prog)
  (compile-code
   (L2->L1 
    (L3->L2
     (L4->L3 
      (L5->L4 L5-prog))))))

(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file 
      (vector-ref (current-command-line-arguments) 0)
    (λ (x) (display (compile-L (read x))))))

(compile-L '((lambda (x) x) 1))




