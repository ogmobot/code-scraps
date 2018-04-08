#!/usr/bin/python
import re, argparse

BIBLE_PATH = "/home/paul/prog/files/bible/kjv.txt"

BOOK_NAMES = [
  # Old Testament
  "Genesis",        "Exodus",         "Leviticus",
  "Numbers",        "Deuteronomy",    "Joshua",
  "Judges",         "Ruth",           "1 Samuel",
  "2 Samuel",       "1 Kings",        "2 Kings",
  "1 Chronicles",   "2 Chronicles",   "Ezra",
  "Nehemiah",       "Esther",         "Job",
  "Psalms",         "Proverbs",       "Ecclesiastes",
  "Song of Solomon","Isiah",          "Jeremiah",
  "Lamentations",   "Ezekiel",        "Daniel",
  "Hosea",          "Joel",           "Amos",
  "Obadiah",        "Jonah",          "Micah",
  "Nahum",          "Habakkuk",       "Zephaniah",
  "Haggai",         "Zechariah",      "Malachi", 
  # New Testament
  "Matthew",        "Mark",           "Luke",
  "John",           "Acts",           "Romans",
  "1 Corinthians",  "2 Corinthians",  "Galatians",
  "Ephesians",      "Philippians",    "Colossians",
  "1 Thessalonians","2 Thessalonians","1 Timothy",
  "2 Timothy",      "Titus",          "Philemon",
  "Hebrews",        "James",          "1 Peter",
  "2 Peter",        "1 John",         "2 John",
  "3 John",         "Jude",           "Revelation"
]

def parse_input(input_string):
  """Transforms e.g. 
      "John 11:34-35"
     into
      (("John", 11, 34),("John", 11, 35))
  """
  if "-" in input_string:
    ref_start, ref_end = input_string.split("-")
  else:
    ref_start, ref_end = input_string, input_string
  m = re.search(r"([1-3 ]*[A-Za-z ]*) ([0-9]*):([0-9]*)", ref_start)
  # print("0:{}\n1:{}\n2:{}\n3:{}".format(m.group(0), m.group(1), m.group(2), m.group(3)))
  result_start = ( m.group(1).strip(), int(m.group(2)), int(m.group(3)) )
  n = re.search(r"([1-3 ]*[A-Za-z ]+)?([0-9]*):*([0-9]*)", ref_end)
  a, b, c = ( n.group(1), n.group(2), n.group(3) )
  if b and not c:
    c = b
    b = ''
  if not a:
    a = m.group(1)
    if not b:
      b = m.group(2)
      if not c:
        c = m.group(3)
  result_end = ( a.strip(), int(b), int(c) )
  return (result_start, result_end)

def print_bible_text(ref_start, ref_end, bible_text):
  start_book = BOOK_NAMES.index(ref_start[0])
  end_book = BOOK_NAMES.index(ref_end[0])
  start_chapter, start_verse = ref_start[1], ref_start[2]
  end_chapter, end_verse = ref_end[1], ref_end[2]
  p = re.compile("[^0-9]1:1[^0-9]+")
  bible_text = bible_text[:p.search(bible_text).start()]
  while start_book > 0:
    print("foo")
    bible_text = bible_text[:p.search(bible_text).start()]
    start_book -= 1
    end_book -= 1
  while end_book >= 0:
    line, bible_text = bible_text.split("\n", 1)
    print(line)
    if p.match(bible_text):
      break
  return

def parse_bible(bible_file):
  text = bible_file.read()
  m = re.findall("""\n\n([^\n]+)\n+1:1 """, text)
  for l in m:
    if l.strip():
      print(l.strip())
      print("===")
  return

def main():
  # result = parse_input(input("Enter a reference: "))
  # print(result)
  bible_text = open(BIBLE_PATH, "r").read()
  print_bible_text(("Exodus", 1, 1), ("Exodus", 1, 1), bible_text)
  return

if __name__=="__main__":
  main()
