#!/bin/bash

racket L5-gen.rkt > test_file.L5
../322-interps/L5 test_file.L5
