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
  movl $101, %eax
pushl $_magic_label_name_swagger_1
pushl %ebp
movl %esp, %ebp
jmp _a_function
_magic_label_name_swagger_1:

  popl   %ebp
  popl   %edi
  popl   %esi
  popl   %ebx
  leave
  ret
  _a_function:
movl $17, %ebx
movl $11, %edx
cmpl %edx, %ebx
setl %al
movzbl %al, %eax
sall $1, %eax
addl $1, %eax
pushl %eax
call print
addl $4, %esp
movl %ebp, %esp
popl %ebp
ret
.size  go, .-go
.section  .note.GNU-stack,"",@progbits
