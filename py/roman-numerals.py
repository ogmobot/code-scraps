DEBUG = False
key_order = "IvVxXlLcCdDmM"
values = [
        ("M", 1000), ("m", 900),
        ("D",  500), ("d", 400),
        ("C",  100), ("c",  90),
        ("L",   50), ("l",  40),
        ("X",   10), ("x",   9),
        ("V",    5), ("v",   4),
        ("I",    1)
]
shorthand = {
    "v": "IV", "x": "IX",
    "l": "XL", "c": "XC",
    "d": "CD", "m": "CM"
}

def valid_numeral(s):
    # D, L and V can each only appear once
    if s.count("D") > 1 or s.count("L") > 1 or s.count("V") > 1:
        if DEBUG: print(f"{s} has invalid number of D/L/V")
        return False
    # M, C and X cannot be equalled by smaller denominations
    if ("I"*10 in s) or ("X"*10 in s) or ("C"*10 in s):
        if DEBUG: print(f"{s} has too many I or X or C")
        return False
    for short_form, long_form in shorthand.items():
        s = s.replace(long_form, short_form)
    # numerals must be in descending order
    for index, symbol in enumerate(s):
        if index == 0: continue
        if key_order.index(s[index-1]) < key_order.index(symbol):
            if DEBUG: print(f"{s} has {s[index-1]} before {symbol}")
            return False
    return True

def find_value(s):
    for short_form, long_form in shorthand.items():
        s = s.replace(long_form, short_form)
    return sum(dict(values).get(c) for c in s)

def minimal_roman(n):
    output = ""
    for symbol, value in values:
        while n >= value:
            n -= value
            output += symbol
    for short_form, long_form in shorthand.items():
        output = output.replace(short_form, long_form)
    return output
