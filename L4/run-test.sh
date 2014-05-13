#!/bin/sh

echo "Running $1.L4 in L4"

../322-interps/L4 $1.L4

echo "Running $1.L3 in L3"

../322-interps/L3 $1.L3
