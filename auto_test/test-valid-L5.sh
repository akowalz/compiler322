#!/bin/bash

while true; do
racket L5-gen.rkt > test_file.L5
echo "testing L5 expression: "
cat test_file.L5

../322-interps/L5 test_file.L5
done
