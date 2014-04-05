### Compilers

test-asm.sh is a script to automate the building and testing of asm files, should make our lives a little easier in the long run and we can in the compilation step eventually too.  Just pass it the filename (without any extensions) and it will create an a.out file and run it. 

So if you have `assembly-code.S` in the directory:

```
$ ./test-asm.sh assembly-code 
### output from linking everything and running it 
```
