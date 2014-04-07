#lang plai
(require racket/file)

(print-only-errors)

(define (read-file-to-s path)
  (file->value path))



(define-type L1-expr
  [register (name symbol?)]
  [numV (n number?)]
  [label-expr (label string?)]
  [arrow-expr (x L1-expr?) (s L1-expr?)]
  [aop-plus (l L1-expr?) (r L1-expr?)]
  [aop-minus (l L1-expr?) (r L1-expr?)]
  [aop-mult (l L1-expr?) (r L1-expr?)]
  [aop-and (l L1-expr?) (r L1-expr?)]
  [sop-left (l L1-expr?) (r L1-expr?)]
  [sop-right (l L1-expr?) (r L1-expr?)]
  [mem-ref-expr (dest L1-expr?) (src L1-expr?) (offset number?)]
  [mem-write-expr (dest L1-expr?) (src L1-expr?) (offset number?)]
  [cmp (c CMP-expr?)]
  [print-expr (t L1-expr?)]
  [goto-expr (dest L1-expr?)])


(define-type CMP-expr
  [cmp-greater (l L1-expr?) (r L1-expr?)]
  [cmp-greater-eql (l L1-expr?) (r L1-expr?)]
  [cmp-eql (l L1-expr?) (r L1-expr?)])

(define (parse-rand rand)
  (match rand 
    [(? number?) (numV rand)]
    [(? symbol?) (if (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$"
                                   (symbol->string rand))
                     (label-expr (string-append "_" (substring (symbol->string rand) 1)))
                     (register rand))]))

(define (parse expr)
  (match expr
    [`(eax <- (print ,t)) (print-expr (parse-rand t))]
    [`(,dest <- (mem ,addr ,offset)) (mem-ref-expr (parse-rand dest)
                                                   (parse-rand addr)
                                                   offset)]
    [`((mem ,dest ,offset) <- ,src) (mem-write-expr (parse-rand dest)
                                                    (parse-rand src)
                                                    offset)]
    [`(,x <- ,y) (arrow-expr (parse-rand x) (parse-rand y))]
    [`(,x += ,y) (aop-plus (parse-rand x) (parse-rand y))]
    [`(,x -= ,y) (aop-minus (parse-rand x) (parse-rand y))]
    [`(,x *= ,y) (aop-mult (parse-rand x) (parse-rand y))]
    [`(,x &= ,y) (aop-and (parse-rand x) (parse-rand y))]
    [`(,x <<= ,y) (sop-left (parse-rand x) (parse-rand y))]
    [`(,x >>= ,y) (sop-right (parse-rand x) (parse-rand y))]
    [`(goto ,label) (goto-expr (parse-rand label))]))

; L1-expr -> String?
(define (compile expr)
  (type-case L1-expr expr
    [register (name) (format "%~A" name)]
    [numV (n) (format "$~A" n)]
    [label-expr (label) (format "$~A" label)]
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
    [mem-ref-expr (dest src offset) (format "movl ~A(~A), ~A\n"
                                      offset (compile src) (compile dest))]
    [mem-write-expr (dest src offset) (format "movl ~A, ~A(~A)\n"
                                      (compile src) offset (compile dest))]
    [print-expr (t) (format "pushl ~A\ncall print\naddl $4, %esp" (compile t))]
    [goto-expr (label) (format "jmp ~A\n" (label-expr-label label))]
    ))

(define header ".text\n.globl go\n.type go, @function\ngo:\n")
(define footer ".size  go, .-go\n.section  .note.GNU-stack,\"\",@progbits\n")
(define main-prefix 
  "
  pushl   %ebp
  movl    %esp, %ebp

  pushl   %ebx
  pushl   %esi
  pushl   %edi
  pushl   %ebp

  movl    %esp, %ebp
  ")
(define main-suffix 
  "
  popl   %ebp
  popl   %edi
  popl   %esi
  popl   %ebx
  leave
  ret
  ")
(define (compile-code code)
  (let ([main-expr (first code)]
        [rest-exprs (rest code)])
      (string-append header
                     main-prefix
                     (foldr string-append "" (map compile (map parse main-expr)))
                     (foldr string-append "" (map compile (map parse rest-exprs)))
                     main-suffix
      footer)))

(test (compile (arrow-expr (register `eax) (numV 5)))
      "movl $5, %eax\n")
(test (compile (aop-plus (register `eax) (numV 6)))
      "addl $6, %eax\n")
(test (compile (sop-right (numV 4) (register `ecx)))
      "sarl %cl, $4\n")
(test (parse-rand ':test_label) (label-expr "_test_label"))
(test (parse-rand 'eax) (register 'eax))
(test (parse `(eax <- (mem ebp 4)))
      (mem-ref-expr (register 'eax) (register 'ebp) 4))
(test (compile (parse `(eax <- (mem edx 4))))
  "movl 4(%edx), %eax\n")
(test (compile (parse `((mem edx 4) <- 11)))
  "movl $11, 4(%edx)\n")
(test (compile (parse `((mem edx 4) <- ebp)))
  "movl %ebp, 4(%edx)\n")
(test (compile (parse `((mem edx 4) <- :test_label)))
  "movl $_test_label, 4(%edx)\n")
(test (compile (parse `(goto :place)))
  "jmp _place\n")


