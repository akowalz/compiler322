.text
.globl go
.type go, @function
go:

  pushl   %ebp
  movl    %esp, %ebp

  pushl   %ebx
  pushl   %esi
  pushl   %edi
  pushl   %ebp

  movl    %esp, %ebp
  movl $89, %esi
pushl $3
pushl %esi
call allocate
addl $8, %esp
movl %eax, %edi
movl $5, %ebx
_loop:
cmpl %esi, %ebx
jl _keep_going
jmp _done
_keep_going:
movl %ebx, %edx
subl $4, %edx
subl $1, %edx
imull $2, %edx
addl %edi, %edx
addl $4, %edx
movl 0(%edx), %edx
movl %ebx, %ecx
subl $2, %ecx
subl $1, %ecx
imull $2, %ecx
addl %edi, %ecx
addl $4, %ecx
movl 0(%ecx), %ecx
addl %ecx, %edx
subl $1, %edx
movl %ebx, %ecx
subl $1, %ecx
imull $2, %ecx
addl $4, %ecx
addl %edi, %ecx
movl %edx, 0(%ecx)
addl $2, %ebx
jmp _loop
_done:
pushl %edi
call print
addl $4, %esp

  popl   %ebp
  popl   %edi
  popl   %esi
  popl   %ebx
  leave
  ret
  .size  go, .-go
.section  .note.GNU-stack,"",@progbits
