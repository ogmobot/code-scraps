#!/usr/bin/python

import re
CARDWIDTH = "63mm"
CARDHEIGHT = "82mm"

class MtgCard:
  def __init__(self):
    name = ""
    cost = ""
    cardtype = ""
    text = ""
    pt = None

def str2card(cardstring):
  lines = cardstring.split("\n")
  result = MtgCard()
  result.name, result.cost = lines.pop(0).split("\t")
  result.cardtype = lines.pop(0)
  if re.match("^[^a-z]*/[^a-z]*$", lines[-1]):
    result.pt = lines.pop()
  result.text = "\n".join(lines)
  return result

def card2tex(card):
  result = r"\fbox{}"
  return result

def main():
  return

if __name__=="__main__":
  main()
