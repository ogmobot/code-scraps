#!/bin/python3

import os
import sys
import requests
import json
import argparse
CACHE_LOCATION = os.environ["HOME"] + "/.spellfetch/"
DND_API = "https://www.dnd5eapi.co/api/"

# Note that this won't be able to find spells from e.g. Elemental Evil
# Player's Companion, although those may be added manually by placing a
# JSON file into the cache.

# Generic utility functions
def remove_personal_name(spellname):
    # Converts "Mordekainen's Magnificent Mansion" to "Magnificent Mansion"
    if spellname.lower() == "mordenkainen's sword":
        return "arcane sword"
    elif spellname.lower() == "nystul's magic aura":
        return "arcanist's magic aura"
    elif spellname.lower() == "bigby's hand":
        return "arcane hand"
    elif spellname.lower() == "jim's magic missile":
        # Doesn't appear in API; take a guess
        return "arcanist's magic missile"
    elif spellname.lower() == "hunter's mark":
        return spellname
    elif spellname.lower() == "heroes' feast":
        # Heroes' Feast doesn't end with "'s" anyway
        return spellname
    words = spellname.split()
    if words and words[0].endswith("'s"):
        words.pop(0)
    return " ".join(words)

def clean_string(s):
    # Converts a string to lowercase, removes apostrophes, and
    # replaces non-alnum chars with '-'
    return "".join((c if c.isalnum() else '-') for c in s.lower() if c != "'").strip("-")

def file_to_list(fp):
    return [line.strip() for line in fp if line.strip()]

# API request functions
def get_spell(spellname):
    cleanname = clean_string(spellname)
    cleanname_generic = clean_string(remove_personal_name(spellname))
    spell = None
    existing_spells = []
    try:
        existing_spells = os.listdir(CACHE_LOCATION)
    except FileNotFoundError:
        os.mkdir(CACHE_LOCATION)
    if cleanname+".json" in existing_spells:
        # Users should be allowed to add custom spells, including spells
        # with personal names, to the cache.
        with open(CACHE_LOCATION+cleanname+".json", "r") as f:
            spell = json.load(f)
    elif cleanname_generic+".json" in existing_spells:
        with open(CACHE_LOCATION+cleanname_generic+".json", "r") as f:
            spell = json.load(f)
    else:
        r = requests.get(DND_API + "spells/" + cleanname_generic)
        if r.status_code == requests.codes.ok:
            spell = r.json()
        if spell:
            with open(CACHE_LOCATION+cleanname_generic+".json", "w") as f:
                json.dump(spell, f)
    if not spell:
        sys.stderr.write(f"Error: could not find the spell named \"{spellname}\".\n")
    return spell

def get_spell_list(stringlist):
    result = []
    for spellname in stringlist:
        spell = get_spell(spellname)
        if spell:
            result.append(spell)
        else:
            # get_spell already warns user that the spell wasn't found
            pass
    if len(stringlist) > len(result):
        sys.stderr.write(f"{len(stringlist)-len(result)} spell(s) not found.\n")
    return result

# Formatting functions
def format_md(spelllist):
    return spelllist

def md2html(text):
    return text

def md2tex(text):
    return text

def tex_topntail(text):
    return text

# Main function
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_file",
        nargs="?",
        type=argparse.FileType("r"),
        default=sys.stdin,
        help="input file (default stdin)")
    parser.add_argument("-o",
            dest="output_filename",
            nargs=1,
            default=[""],
            help="output file (default stdout); should end with .html, .json, .md, or .tex")
    args = parser.parse_args()
    post_process = []
    # post_process is a list of functions to sequentially call on a list of dicts to
    # change it into a string.
    if args.output_filename[0]:
        for suffix, functions in {
            ".md":  [format_md],
            ".html":[format_md, md2html],
            ".tex": [format_md, md2tex, tex_topntail],
        }.items():
            if args.output_filename[0].endswith(suffix):
                post_process.extend(functions)
        output_file = open(args.output_filename[0], "w")
    else:
        output_file = sys.stdout
    if len(post_process) == 0:
        post_process.append((lambda x: json.dumps(x, indent=4)))
    output = get_spell_list(file_to_list(args.input_file))
    for function in post_process:
        output = function(output)
    output_file.write(output)
    return

if __name__=="__main__":
    main()
