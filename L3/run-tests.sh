#!/bin/sh

echo "Running $1.L3 in L3"

../322-interps/L3 $1.L3

echo "Running $1.L2 in L2"

../322-interps/L2 $1.L2
