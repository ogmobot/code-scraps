#!/bin/python3

from sys import argv

def main():
  if len(argv) > 1:
    num = int(argv[1])
  else:
    num = 10

  while num >= 0:
    print(num)
    num -= 1

  return

if __name__=="__main__":
  main()
