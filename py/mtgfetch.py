#!/bin/python3

import requests, sys, argparse, os, ast
SCRYFALL_API = "https://api.scryfall.com/"
CACHE_LOCATION = os.environ["HOME"] + "/.mtgfetch"
global _DEBUG

def slugify(s): # converts string to legal filename
    return "".join([c if c in "abcdefghijklmnopqrstuvwxyz." else "-" for c in s.lower()])

def get_card(cardname):
    try:
        existing_cards = os.listdir(CACHE_LOCATION) # ideally, we'd only need to load this once...
    except FileNotFoundError:
        os.mkdir(CACHE_LOCATION)
        existing_cards = []
    if slugify(cardname+".json") in existing_cards:
        f = open(CACHE_LOCATION + "/" + slugify(cardname+".json"), "rU")
        card = ast.literal_eval(f.read())
        f.close()
    else:
        payload = {"fuzzy":cardname}
        r = requests.get(SCRYFALL_API + "cards/named", params=payload)
        if r.status_code == requests.codes.ok:
            card = r.json()
        else:
            # The "Splinter" vs "Splinter Twin" case
            payload = {"exact":cardname}
            r = requests.get(SCRYFALL_API + "cards/named",params=payload)
            if r.status_code == requests.codes.ok:
                card = r.json()
            else:
                card = None
        if card and ("name" in card):
            f = open(CACHE_LOCATION + "/" + slugify(card["name"]+".json"), "w")
            f.write(str(card))
            f.close()
    return card

def format_card(raw_card):
    '''Writes the name, mana cost, type, and oracle text of a card.'''
    if "card_faces" in raw_card:
      faces = raw_card.get("card_faces", [])
    else:
      faces = [raw_card]
    results = []
    for card in faces:
        output_text = ""
        output_text += "{}".format(card["name"])
        if "mana_cost" in card:
            output_text += "\t{}".format(card["mana_cost"])
        output_text += "\n{}".format(card["type_line"])
        if "oracle_text" in card:
            output_text += "\n{}".format(card["oracle_text"])
        if "power" in card:
            output_text += "\n{}/{}".format(card["power"], card["toughness"])
        if "loyalty" in card:
            output_text += "\n{}".format(card["loyalty"])
        # Known bug: Scryfall doesn't record the starting loyalty
        #     of the planeswalkers printed in Magic Origins.
        results.append(output_text)
    return "\n//\n".join(results)
    
def interactive(s="", output_file=sys.stdout):
    '''Interactive or command-line argument mode.'''
    if not s:
        s = input("Enter a card name: ")
    card = get_card(s)
    if not card:
        if _DEBUG:
            print("Couldn't find a unique card.")
        sys.stderr.write("Could not identify card.\n")
        return 1
    if _DEBUG:
        print("Found card: {}".format(card["name"]))
    output_file.write(format_card(card))
    output_file.write("\n")
    return 0

def batch(input_file=sys.stdin, output_file=sys.stdout, sep=""):
    '''Batch mode.'''
    output = []
    first_entry = True
    line = input_file.readline().strip()
    while line:
        if not line[0].isalpha():
            line = input_file.readline().strip()
            continue
        sys.stderr.write("Getting card {}...\n".format(line))
        card = get_card(line)
        if card:
            sys.stderr.write("Found card {}.\n".format(card["name"]))
            if first_entry:
                first_entry = False
            else:
                output_file.write(sep+"\n")
            output_file.write(format_card(card))
            output_file.write("\n")
        else:
            sys.stderr.write("Failed to find \"{}\".\n".format(line))
        line = input_file.readline().strip()
    return 0

def main():
    global _DEBUG
    _DEBUG = False

    parser = argparse.ArgumentParser()
    parser.add_argument("input_file",
            nargs="?",
            type=argparse.FileType("r"),
            default=sys.stdin,
            help="input file (default stdin)")
    parser.add_argument("-o",
            dest="output_file",
            nargs=1,
            type=argparse.FileType("w"),
            default=[sys.stdout],
            help="output file (default stdout)")
    parser.add_argument("-s",
            dest="sep",
            nargs=1,
            default=[""],
            help="separator for output")
    parser.add_argument("-c",
            dest="cardname",
            metavar="card_name",
            nargs=1,
            default=[""],
            help="specify card name")
    parser.add_argument("--DEBUG",
            dest="DEBUG",
            action="store_const",
            const=True,
            default=False,
            help="enable debug output")
    args = parser.parse_args()
    _DEBUG = args.DEBUG
    cardname = args.cardname[0]
    if cardname:
        # single-card mode
        interactive(cardname, args.output_file[0])
    else:
        # batch mode
        batch(args.input_file, args.output_file[0], args.sep[0])
    return 0

if __name__=="__main__":
    main()
