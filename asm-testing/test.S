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


  movl $11, %ecx
  movl $13, %ebx
  cmpl %ecx, %ebx
  setl %al
  movzbl %al, %eax

  pushl %eax
  call print
  addl $4, %esp

  popl   %ebp
  popl   %edi
  popl   %esi
  popl   %ebx

  leave
  ret

  .size go, .-go
  .section  .note.GNU-stack,"",@progbits
