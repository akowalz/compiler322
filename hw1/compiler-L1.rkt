#lang plai
(require racket/file)
(require racket/cmdline)

(print-only-errors)

(define (read-file-to-s path)
  (file->value path))

(define label-counter 0)

(define (gen-label)
  (begin (set! label-counter (+ 1 label-counter))
  (string-append "_magic_label_name_swagger_" (number->string label-counter))))


(define-type L1-expr
  [register (name symbol?)]
  [numV (n number?)]
  [label-expr (label string?)]
  [label-target (label string?)]
  [arrow-expr (x L1-expr?) (s L1-expr?)]
  [aop-plus (l L1-expr?) (r L1-expr?)]
  [aop-minus (l L1-expr?) (r L1-expr?)]
  [aop-mult (l L1-expr?) (r L1-expr?)]
  [aop-and (l L1-expr?) (r L1-expr?)]
  [sop-left (l L1-expr?) (r L1-expr?)]
  [sop-right (l L1-expr?) (r L1-expr?)]
  [mem-ref-expr (dest L1-expr?) (src L1-expr?) (offset number?)]
  [mem-write-expr (dest L1-expr?) (src L1-expr?) (offset number?)]
  [print-expr (t L1-expr?)]
  [goto-expr (dest L1-expr?)]
  [cjump-expr (a L1-expr?) (op symbol?) (b L1-expr?) (l-true L1-expr?) (l-false L1-expr?)]
  [cmp-store (dest L1-expr?) (a L1-expr?) (b L1-expr?)  (op symbol?)]
  [allocate-expr (t L1-expr?) (t2 L1-expr?)]
  [array-error-expr (t L1-expr?) (t2 L1-expr?)]
  [call-expr (s L1-expr?)]
  [tail-call (u L1-expr?)]
  [return-expr]
  )


(define (cjump-instr op)
  (case op
    ('< "jl")
    ('<= "jle")
    ('= "je")))

(define (compare-on-op op a b)
  (case op
    ('< (< a b))
    ('<= (<= a b))
    ('= (equal? a b))))

(define (small-reg reg)
  (case (register-name reg)
    ('eax (register 'al))
    ('ecx (register 'cl))
    ('edx (register 'dl))
    ('ebx (register 'bl))))


(define (parse-rand rand)
  (match rand 
    [(? number?) (numV rand)]
    [(? symbol?) (if (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$"
                                   (symbol->string rand))
                     (label-expr (string-append "_" (substring (symbol->string rand) 1)))
                     (register rand))]))

(define (parse expr)
  (match expr
    [(? symbol?) (label-target (string-append "_" (substring (symbol->string expr) 1)))]
    [`(call ,u) (call-expr (parse-rand u))]
    [`(tail-call ,u) (tail-call (parse-rand u))]
    [`(return) (return-expr)]
    [`(,dest <- ,a ,op ,b) (cmp-store (parse-rand dest)
                                      (parse-rand a)
                                      (parse-rand b)
                                      op)]
    [`(eax <- (print ,t)) (print-expr (parse-rand t))]
    [`(eax <- (allocate ,t1 ,t2)) (allocate-expr (parse-rand t1) (parse-rand t2))]
    [`(eax <- (array-error-expr ,t1 ,t2)) (array-error-expr (parse-rand t1) (parse-rand t2))]
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
    [`(goto ,label) (goto-expr (parse-rand label))]
    [`(cjump ,l ,op ,r ,l1 ,l2) (cjump-expr (parse-rand l) op (parse-rand r) 
                                            (parse-rand l1) (parse-rand l2))]
    ))


; L1-expr -> String?
(define (compile expr)
  (type-case L1-expr expr
    [register (name) (format "%~A" name)]
    [numV (n) (format "$~A" n)]
    [label-target (label) (format "~A:\n" label)]
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
    ;[cmp (c) "" ]
    [mem-ref-expr (dest src offset) (format "movl ~A(~A), ~A\n"
                                      offset (compile src) (compile dest))]
    [mem-write-expr (dest src offset) (format "movl ~A, ~A(~A)\n"
                                      (compile src) offset (compile dest))]
    [print-expr (t) (format "pushl ~A\ncall print\naddl $4, %esp\n" (compile t))]
    [allocate-expr (t1 t2) (format "pushl ~A\npushl ~A\ncall allocate\naddl $8, %esp\n"
                           (compile t2)
                           (compile t1))]
    [array-error-expr (t1 t2) (format "pushl ~A\npushl ~A\ncall print-error\naddl $8, %esp\n"
                           (compile t2)
                           (compile t1))]
    [goto-expr (label) (format "jmp ~A\n" (label-expr-label label))]
    [cjump-expr (a op b l1 l2) (cond [(and (numV? a) (numV? b)) 
                                           (format "jmp ~A\n"
                                              (if (compare-on-op op (numV-n a) (numV-n b))
                                                  (label-expr-label l1)
                                                  (label-expr-label l2)))]
                                     [(numV? a) (format "cmpl ~A, ~A\n~A ~A\njmp ~A\n"
                                                (compile a) (compile b)
                                                (cjump-instr op)
                                                (label-expr-label l2)
                                                (label-expr-label l1))]
                                     [else  (format "cmpl ~A, ~A\n~A ~A\njmp ~A\n"
                                                (compile b) (compile a)
                                                (cjump-instr op)
                                                (label-expr-label l1)
                                                (label-expr-label l2))])]
    [cmp-store (dest a b op) (cond [(and (numV? a) (numV? b))
                                    (format "movl ~A, ~A\n"
                                    (if (compare-on-op op (numV-n a) (numV-n b))
                                        "$1" "$0")
                                    (compile dest))]
                                   [(numV? a) 
                                     (format "cmpl ~A, ~A\nsetl ~A\nmovzbl ~A, ~A\naddl $1, ~A\nandl $1, ~A\n"
                                     (compile a) (compile b) 
                                     (compile (small-reg dest))
                                     (compile (small-reg dest)) 
                                     (compile dest)
                                     (compile dest)
                                     (compile dest))]
                                   [else (format "cmpl ~A, ~A\nsetl ~A\nmovzbl ~A, ~A\n"
                                          (compile b) (compile a)
                                          (compile (small-reg dest))
                                          (compile (small-reg dest)) (compile dest))])]
    [call-expr (u) (let ([new-label (gen-label)])
                        (format "pushl $~A\npushl %ebp\nmovl %esp, %ebp\njmp ~A\n~A:\n"
                        new-label
                        (if (register? u)
                            (format "*~A" compile u)
                            (label-expr-label u))
                        new-label))]
    [tail-call (u) (format "movl %ebp, %esp\njmp ~A\n"
                          (if (register? u)
                              (format "*~A" compile u)
                              (label-expr-label u)))]
    [return-expr () "movl %ebp, %esp\npopl %ebp\nret\n"]
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
                     main-suffix
                     (foldr string-append "" (map compile-fn rest-exprs))
      footer)))

(define (compile-fn fn)
  (let ([fn-name (first fn)]
        [fn-body (rest fn)])
    (string-append "_" (substring (symbol->string fn-name) 1) ":\n"
      (foldr string-append "" (map compile (map parse fn-body))))))


(define (compile-L1 path)
  (compile-code (file->value path)))

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

(test (compile (parse `(cjump eax < ebx :here :there)))
  "cmpl %ebx, %eax\njl _here\njmp _there\n")

(test (parse `(cjump eax < ebx :here :there))
  (cjump-expr (register 'eax) '< (register 'ebx) (label-expr "_here") (label-expr "_there")))

(test (compile (parse `(cjump 13 = 15 :yes :no)))
  "jmp _no\n")

(test (compile (parse `(cjump 17 <= ebx :true :false)))
  "cmpl $17, %ebx\njle _false\njmp _true\n")


(test (parse `(eax <- edx = ebx))
  (cmp-store (register 'eax) (register 'edx) (register 'ebx) '=))

(test (compile (parse `(eax <- edx < ebx)))
  "cmpl %ebx, %edx\nsetl %al\nmovzbl %al, %eax\n")

(test (compile (parse  `(edx <- 17 = 17)))
  "movl $1, %edx\n")

(test (compile (parse  `(ecx <- 15 < ebx)))
  "cmpl $15, %ebx\nsetl %cl\nmovzbl %cl, %ecx\naddl $1, %ecx\nandl $1, %ecx\n")



; this is so janky! but it's all cool man
(when (= (vector-length (current-command-line-arguments)) 1)
  (display (compile-L1 (vector-ref (current-command-line-arguments) 0))))

