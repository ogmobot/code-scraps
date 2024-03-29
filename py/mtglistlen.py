#!/bin/python3

import requests, sys, argparse, time, random
SCRYFALL_API = "https://api.scryfall.com/"
global _DEBUG

def random_wait():
    #time.sleep(random.random())
    return

def get_creature_subtypes():
    '''Returns a list of all creature subtypes.'''
    r = requests.get(SCRYFALL_API + "catalog/creature-types")
    if r.status_code == requests.codes.ok:
        catalogue = r.json()
        result = catalogue["data"]
    else:
        result = []
    return result

def get_creature_balance(subtype):
    '''Returns a tuple (a, b) where
       - a is the number of creatures with the subtype
       - b is the number of legendary creatures with the subtype
    '''
    payload_a = {"q":"t:creature t:{}".format(subtype)}
    r = requests.get(SCRYFALL_API + "cards/search", params=payload_a)
    if r.status_code == requests.codes.ok:
        result = r.json()
        len_a = result["total_cards"]
        if result.get("warnings", False):
          sys.stderr.write(result["warnings"])
    else:
        len_a = 0
    random_wait()
    payload_b = {"q":"t:legendary t:creature t:{}".format(subtype)}
    r = requests.get(SCRYFALL_API + "cards/search", params=payload_b)
    if r.status_code == requests.codes.ok:
        result = r.json()
        len_b = result["total_cards"]
        if result.get("warnings", False):
          sys.stderr.write(result["warnings"])
    else:
        len_b = 0
    random_wait()
    return (len_a, len_b)

def get_all_legends(subtype_list, verbose=False):
    output = "subtype\ttotal\tlegends\n"
    for subtype in subtype_list:
        if verbose:
            sys.stderr.write("Finding {}...\n".format(subtype))
        result = get_creature_balance(subtype)
        output += "{}\t{}\t{}\n".format(subtype, *result)
    return output

def get_card(cardname):
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
    return card

def format_card(card):
    '''Writes the name, mana cost, type, and oracle text of a card.'''
    output_text = ""
    output_text += "{}".format(card["name"])
    if "mana_cost" in card:
        output_text += "\t{}".format(card["mana_cost"])
    output_text += "\n{}".format(card["type_line"])
    if "oracle_text" in card:
        output_text += "\n{}".format(card["oracle_text"])
    if "power" in card:
        output_text += "\n{}/{}".format(card["power"], card["toughness"])
    return output_text

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

def main_orig():
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

def main():
    subtype_list = get_creature_subtypes()
    output = get_all_legends(subtype_list, True)
    sys.stdout.write(output)

if __name__=="__main__":
    main()
