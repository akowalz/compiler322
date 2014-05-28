#lang racket
(require rackunit racket/set)

(check-equal? 
 ((λ(x)
    (+ x 
       ((λ(x) x) 2)
       )) 4) 6)

(check-equal? 
 (let ([y 3])
    ((λ(x)
      (+ y 
       ((λ() x))
       )) 4)) 7)
