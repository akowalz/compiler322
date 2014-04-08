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
  pushl $_magic_label_name_swagger_1
pushl %ebp
movl %esp, %ebp
jmp _L_16
_magic_label_name_swagger_1:

  popl   %ebp
  popl   %edi
  popl   %esi
  popl   %ebx
  leave
  ret
  _L_16:
movl %esi, %esi
movl %edi, %edi
movl $401, %eax
pushl $_magic_label_name_swagger_2
pushl %ebp
movl %esp, %ebp
jmp _sum_even
_magic_label_name_swagger_2:
movl %eax, %ebx
pushl %ebx
call print
addl $4, %esp
movl %eax, %eax
movl %esi, %esi
movl %edi, %edi
movl %ebp, %esp
popl %ebp
ret
_sum_even:
addl $-4, %esp
movl %eax, -4(%ebp)
cmpl $1, %eax
je _L_17
jmp _L_18
_L_18:
andl $3, %eax
cmpl $3, %eax
je _L_20
jmp _L_19
_L_20:
movl -4(%ebp), %eax
subl $2, %eax
movl %ebp, %esp
jmp _sum_even
_L_19:
movl -4(%ebp), %eax
subl $2, %eax
pushl $_magic_label_name_swagger_3
pushl %ebp
movl %esp, %ebp
jmp _sum_even
_magic_label_name_swagger_3:
movl -4(%ebp), %ebx
addl %ebx, %eax
subl $1, %eax
movl %ebp, %esp
popl %ebp
ret
_L_17:
movl $1, %eax
movl %ebp, %esp
popl %ebp
ret
.size  go, .-go
.section  .note.GNU-stack,"",@progbits
