#lang racket
(require "L1.rkt"
         "L2.rkt"
         "L3.rkt"
         "L4.rkt"
         "L5.rkt"
         rackunit)


(define (compile-L L5-prog)
  (L1->x86
   (L2->L1 
    (L3->L2
     (L4->L3 
      (L5->L4 L5-prog))))))

(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file 
      (vector-ref (current-command-line-arguments) 0)
    (λ (x) (display (compile-L (read x))))))


(when (not (string? (compile-L '((lambda (x) x) 1))))
  (error 'Lc "Compile-L doesn't seem to be working!"))




