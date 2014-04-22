test-asm.sh is a script to automate the building and testing of asm files, should make our lives a little easier in the long run and we can in the compilation step eventually too.  Just pass it the filename (without any extensions) and it will create an a.out file and run it. 

So if you have `assembly-code.S` in the directory:

```
$ ./test-asm.sh assembly-code 
### output from linking everything and running it 
```

# Assignment 1 Notes 

## Basic template for compiling


All files must begin with 

```
  .text
  .globl go
  .type go, @function
go:
```
and end with 

```
  .size go, .-go
  .section  .note.GNU-stack,"",@progbits
```

So, our `compile` function should always put these things at the start and end (using string-append probably).

### Compiling "Main"

The first element in the L1 instruction list is the `main` function, so it must begin with the "Main Prefix" and end with the "Main Suffix".

So, we grab the first element of the L1 code (ie L1 "main", (i ...) ), append the prefix: 

```
  pushl   %ebx
  pushl   %esi
  pushl   %edi
  pushl   %ebp

  movl    %esp, %ebp
```

compile all the instructions in main sequentially, and then append that to the suffix:

```
  popl   %ebp
  popl   %edi
  popl   %esi
  popl   %ebx

  leave
  ret
```

### Compiling the L1 level functions

The functions in an L1 program have the form `(label i ...)`.  The label is the the name of the function, the `i`s make up the body of the function.

Compiling functions of this form is easy, we just generate a label with the same name as the function (maybe with an underscore instead of a colon) and then sequentially compile each function in the body of the function.


So, we compile L1-main, described above, then we look to the rest of the list, which will all have the form `(label i ...)`, and we go through them one by one making the appropriate transformations.

## Making function calls

### Calls into runtime.c

Making calls to the `runtime.c` functions is very straightforward, and described well in the lecture notes.  The arguments will already be in the registers for us thanks to the L1 programmer. The x86 `call` instruction will also automatically store the result for us.


Here's how to compile the runtime calls:

* (eax <- (print t)) =>

```
pushl <arg1> ;; will be a register or a number
call print
addl $4,%esp
```

* (eax <- (allocate t t)) =>

```
pushl <arg2>
pushl <arg1>
call allocate
addl $8,%esp
```

* (eax <- array-error t t) =>
This one wasn't in the notes but I assume it will also have the form:

```
pushl <arg2>
pushl <arg1>
call allocate
addl $8,%esp
```

### Calls to L1 functions `(label i ... )`

This slightly more complicated, details all are all in the notes, but here's an attempt to make it more clear what the L1 programmer has to write and what we have have to generate.


**A function call of the form (call s)**

The L1 programmer is responsible for putting the arguments in the registers, we don't have to do that.

What the compiler has to do is jump around the code correctly.  This is done by creating a new label to be used as the return address and placing it right underneath the call.  So, `(call s)` becomes =>

```
pushl $<new-label>  // freshly minted label, ie return address within main
pushl %ebp
movl %esp, %ebp
jmp <s>  // the converted version of s (%, $, etc)
<new-label>:  // when the function returns, we'll end up here!
```

**Returning from a function**

The L1 programmer is responsible for putting the result into %eax and calling L1's (return).

The compiler is responsible for the following when `(return)` is called (from the notes) =>

```
movl %ebp, %esp
popl %ebp
ret
```

A tail call is very similar but gets translated to this, I think we're going to have to pass the value of down somehow, should become apparent when we start coding =>

```
movl %ebp, %esp
jmp <s>  // the converted version of s
```

## Misc

Above is all the hard stuff.  Other misc. stuff we have to do:

Various pieces of the L1 grammar and their translations

**label, ex `(:sum_even)`**=>

`_sum_even`

**(goto label), ex `(goto :sum_even)`** =>

`jmp _sum_even`


**(cx <- t cmp t) ex `(%eax <- %ecx < %edx)`** =>

t's can be registers or nums, this is straight from the notes:

If the second argument is a register:
```
cmpl %ecx, %ebx
setl %al
movzbl %al, %eax
```

If they are numbers, we need to figure out the result of the comparison at compile time and put 1 in %eax for true, and 0 in %eax for false.  Should be something like =>


```
movl $1, %eax
```
