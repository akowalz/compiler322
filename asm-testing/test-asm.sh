#!/bin/bash
as --32 -o $1.o $1.S
gcc -m32 -o a.out $1.o runtime.o
./a.out