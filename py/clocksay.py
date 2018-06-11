#!/bin/python

import sys

NUMBERS = {
         0: "oh",
         1: "one",
         2: "two",
         3: "three",
         4: "four",
         5: "five",
         6: "six",
         7: "seven",
         8: "eight",
         9: "nine",
        10: "ten",
        11: "eleven",
        12: "twelve",
        13: "thirteen",
        14: "fourteen",
        15: "fifteen",
        16: "sixteen",
        17: "seventeen",
        18: "eighteen",
        19: "nineteen",
        20: "twenty",
        30: "thirty",
        40: "forty",
        50: "fifty",
    }

def say_num(n, hour=False):
    '''Returns the English name of <n>, an integer betwen 0 and 59.
       For numbers less than 10, returns "oh <n>".'''
    if n in NUMBERS and (n >= 10 or hour):
        # case: 10-19, 20, 30, 40, 50
        return NUMBERS.get(n)
    else:
        result = ""
        result += NUMBERS.get(10*(n//10),"oughty")
        result += " "
        result += NUMBERS.get(n%10,"fimp")
        return result

def time_to_words(t):
    try:
        hour, minute = [int(x) for x in t.split(":", 1)]
    except:
        sys.stderr.write("Incorrect time format.\n")
        return
    suffix = " pm" if hour >= 12 else " am"
    hour = hour % 12 if hour % 12 else 12
    return "It's {}{}{}".format(
            say_num(hour, True),
            " "+say_num(minute) if minute else "",
            suffix
            )

def main():
    if "-i" in sys.argv:
        t = sys.stdin.readline().strip()
    else:
        import time
        t = time.strftime("%H:%M")
    s = time_to_words(t)
    print(s)
    return

if __name__ == "__main__":
    main()
