from string import ascii_letters, ascii_uppercase, ascii_lowercase
from random import shuffle
string_in = "."
while len(string_in.strip())>0:
    # get raw input
    string_in = input("Enter a string to encode.\n").strip()
    # make encoding table
    if all(c not in ascii_lowercase for c in string_in):
        key = list(ascii_uppercase)
    else:
        key = list(ascii_letters)
    shuffle(key)
    key *= 2 # so that the key's length is at least that of ascii_letters
    # produce output
    string_out = ""
    for c in string_in:
        if c in ascii_letters:
            string_out += key[ascii_letters.index(c)]
        else:
            string_out += c
    print(string_out)
