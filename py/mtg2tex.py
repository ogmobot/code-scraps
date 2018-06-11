#!/usr/bin/python

import re
CARDWIDTH = "63mm"
CARDHEIGHT = "82mm"

class MtgCard:
  def __init__(self):
    self.name = ""
    self.faces = []
  def addFace(self, face):
    face.parent = self
    self.faces.append(face)

class MtgCardFace:
  def __init__(self):
    self.name = "<Placeholder name>"
    self.cost = None
    self.typeline = "<Placeholder typeline>"
    self.text = ""
    self.pt = None
    pass

def str2card(cardstring):
  resultcard = MtgCard()
  faces = cardstring.split("\n//\n")
  for face in faces:
    lines = face.split("\n")
    result = MtgCardFace()
    topline = lines.pop(0)
    if topline.count("\t") == 1:
      result.name, result.cost = topline.split("\t")
    else:
      result.name = topline
    result.typeline = lines.pop(0)
    if re.match("^[^a-z]*/[^a-z]*$", lines[-1]):
      result.pt = lines.pop()
    result.text = "\n".join(lines)
    resultcard.addFace(result)
  resultcard.name = " // ".join([f.name for f in resultcard.faces])
  return resultcard

def card2tex(card):
  result = r"\fbox{}"
  return result

def main():
  return

if __name__=="__main__":
  main()
