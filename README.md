# L to x86 Compiler

This is a compiler for a simple Scheme-like language into x86 assembly code, which can be run Intel x86 architecture.
This is my fork of a project that Nathan Yeazel [(@nateyeazel)](https://github.com/nateyeazel) and I did for Robby
Findler's course 'Compiler Construction' at Northwestern in the Spring of 2014.

The compiler takes an input a program of the following Scheme-like grammar:

```
e ::= (lambda (x ...) e)
    | x
    | (let ([x e]) e)
    | (letrec ([x e]) e)
    | (if e e e)
    | (new-tuple e ...)
    | (begin e e)
    | (e e ...) ;; application expression
    | prim
    | num

prim ::= biop
       | pred
       | print
       | new-array
       | aref
       | aset
       | alen

biop ::= + | - | * | < | <= | =
pred ::= number? | a?


# source: http://www.eecs.northwestern.edu/~robby/courses/322-2014-spring/lecture10.txt
```

and produces valid x86 assembly.  As you can see, the language allows for lambdas, variables (`let`), recursion through mutation (`letrec`), arrays,
tuples, and a variety of primitive operations.

## Methodology

The compiler works by compiling down from the high-level language shown above (dubbed L5) through five intermediate
languages (dubbed Ls 4-1) and finally into assembly.  A basic summary of the processes involved is as follows:


#### L5 -> L4

Higher order function transformation.

#### L4 -> L3

Linearization (creating explicit order of evaluaton)

#### L3 -> L2

Transformation from higher-level functions to x86 instructions (use basic
operators, etc)

#### L2 -> L1

Register allocation

#### L1 - x86

Formatting of the assembly, headers, linking, stack preparation

## Using the compiler

If you'd like to try out the compiler yourself you'll need Racket installed, and to run the output you'll need to have
access to GNU Linux.  The binary to run the compiler is located in the `Lc` directory and named itself `Lc`, and should
be called with a path to an L5 program as its first and only input.

```
# from the project root
$ Lc/Lc ./path/to/prog.L5
```

This will write a file called L5.asm and attempt to run it using default compilation options.


The final compiler is simply the composition of the previously listed functions, all of which can also be used in a
similar way.

(Literally a composition, look at this thing:)

```racket
(define (compile-L L5-prog)
  (L1->x86
   (L2->L1
    (L3->L2
     (L4->L3
      (L5->L4 L5-prog))))))
```
