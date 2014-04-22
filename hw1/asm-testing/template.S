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

  

  popl   %ebp
  popl   %edi
  popl   %esi
  popl   %ebx

  leave
  ret

  .size go, .-go
  .section  .note.GNU-stack,"",@progbits
