#lang plai
(require racket/file)

(print-only-errors)

(define (read-file-to-s path)
  (file->value path))



(define-type L1-expr
  [register (name symbol?)]
  [numV (n number?)]
  [arrow-expr (x L1-expr?) (s L1-expr?)]
  [aop-plus (l L1-expr?) (r L1-expr?)]
  [aop-minus (l L1-expr?) (r L1-expr?)]
  [aop-mult (l L1-expr?) (r L1-expr?)]
  [aop-and (l L1-expr?) (r L1-expr?)]
  [sop-left (l L1-expr?) (r L1-expr?)]
  [sop-right (l L1-expr?) (r L1-expr?)]
  [mem-expr (x L1-expr?) (n L1-expr?)]
  [cmp (c CMP-expr?)]
  [label-expr (label symbol?)]
  [goto-expr (dest L1-expr?)])


(define-type CMP-expr
  [cmp-greater (l L1-expr?) (r L1-expr?)]
  [cmp-greater-eql (l L1-expr?) (r L1-expr?)]
  [cmp-eql (l L1-expr?) (r L1-expr?)])


(define (parse expr)
  (match expr
    [(? number?) (numV expr)]
    [(? symbol?) (register expr)]
    [`(mem ,x ,y) (mem-expr (parse x) (parse y))]
    [`(,x <- ,y) (arrow-expr (parse x) (parse y))]
    [`(,x += ,y) (aop-plus (parse x) (parse y))]
    [`(,x -= ,y) (aop-minus (parse x) (parse y))]
    [`(,x *= ,y) (aop-mult (parse x) (parse y))]
    [`(,x &= ,y) (aop-and (parse x) (parse y))]
    [`(,x <<= ,y) (sop-left (parse x) (parse y))]
    [`(,x >>= ,y) (sop-right (parse x) (parse y))]))

; L1-expr -> String?
(define (compile expr)
  (type-case L1-expr expr
    [register (name) (format "%~A" name)]
    [numV (n) (format "$~A" n)]
    [arrow-expr (x s) (format "movl ~A, ~A\n" (compile s) (compile x))]
    [aop-plus (l r) (format "addl ~A, ~A\n" (compile r) (compile l))]
    [aop-minus (l r) (format "subl ~A, ~A\n" (compile r) (compile l))]
    [aop-mult (l r) (format "imull ~A, ~A\n" (compile r) (compile l))]
    [aop-and (l r) (format "andl ~A, ~A\n" (compile r) (compile l))]
    [sop-left (l r) (format "sall ~A, ~A\n"
                            (if (register? r)
                                "%cl"
                                (compile r))
                            (compile l))]
    [sop-right (l r) (format "sarl ~A, ~A\n"
                            (if (register? r)
                                "%cl"
                                (compile r))
                            (compile l))]
    [cmp (c) "" ]
    [mem-expr (x n) ""]))

(define (compile-code code)
  (let ([exprs (first code)])
    (begin
      (compile (parse (first exprs)))
      (compile (parse (first (rest exprs)))))))

(test (compile (arrow-expr (register `eax) (numV 5)))
      "movl $5, %eax\n")
(test (compile (aop-plus (register `eax) (numV 6)))
      "addl $6, %eax\n")
(test (compile (sop-right (numV 4) (register `ecx)))
      "sarl %cl, $4\n")
(test (compile (cmp (cmp-greater (register 'ebx) (register 'ecx))))
      "cmpl %ecx, %ebx\nsetl %al\nmovzbl %al, %eax\n")
(test (compile (arrow-expr (register `eax) (mem-expr (register `ebx) (numV 12))))
      "movl 12(%ebx), %eax")




