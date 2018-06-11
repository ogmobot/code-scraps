#!/bin/sh
# 64-bit unsigned ints can only find fib(93) or less.
# Assembly
echo "x86_64 Assembly"
echo "1 million loops"
{ time echo 1000000 | ~/prog/x86_64/fib > /dev/null ; } 2>&1
cat ~/prog/x86_64/fibonacci.S ~/prog/x86_64/fibwrap.c | wc
echo "----------"

echo "C"
echo "1 million loops"
{ time echo 1000000 | ~/prog/c/fib > /dev/null ; } 2>&1
wc ~/prog/c/fib.c
echo "----------"

echo "Python"
echo "1 million loops"
{ time echo 1000000 | ~/prog/py/fib.py > /dev/null ; } 2>&1
wc ~/prog/py/fib.py
echo "----------"

echo "Scheme"
echo "1 million loops"
{ time echo 1000000 | ~/prog/scm/load.sh ~/prog/scm/fib.scm > /dev/null ; } 2>&1
wc ~/prog/scm/fib.scm
echo "----------"

echo "BrainF***"
echo "30 loops"
{ time echo 30 | ~/prog/bf/bfi ~/prog/bf/fib-1.b > /dev/null ; } 2>&1
wc ~/prog/bf/fib-1.b
