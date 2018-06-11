#!/bin/python

a = int(input())-1
x = 0
y = 1
for i in range(a):
  z = x + y
  x = y
  y = z
print(y)
