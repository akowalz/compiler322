#lang racket

(define (kill instr)
  (match instr
    [`(,x <- (mem ,y ,n)) ]
    [`((mem ,y ,n) <- ,s) ]
    [`(eax <- (allocate ,t1 ,t2)) ]
    [`(eax <- (array-error ,t1 ,t2)) ]
    [`(,x <- ,s) ]
    [`(,x ,op ,t) ]
    [`(,cx <- ,t1 ,cop, ,t2) ]
    [(? symbol?) ]
    [`(goto ,label) ]
    [`(cjump ,t1 ,cop ,t2 ,label1 ,label2) ]
    [`(call ,u) ]
    [`(tail-call ,u) ]
    [`(return) ]
    [`(eax <- (print ,t)) ]
    [else (error 'parse "Expression didn't conform to L1 grammar")]))
    