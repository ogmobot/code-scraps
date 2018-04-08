#!/bin/python

import sys

maxnum = int(sys.argv[1])
numbers = list(range(maxnum))
numbers[1] = 0

for n in numbers:
    if n == 0:
        continue
    else:
        i = 2*n
        while i < maxnum:
            numbers[i] = 0
            i += n
for n in numbers:
    if n:
        print(n)
