#!/bin/python3

import os
import sys
import requests
import json
CACHE_LOCATION = os.environ["HOME"] + "/.spellfetch/"
DND_API = "https://www.dnd5eapi.co/api/"

# Note that this won't be able to find spells from e.g. Elemental Evil
# Player's Companion

def remove_personal_names(spellname):
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

def get_spell(spellname):
    spellname = clean_string(spellname)
    # check cache
    existing_spells = []
    try:
        existing_spells = os.listdir(CACHE_LOCATION)
    except FileNotFoundError:
        os.mkdir(CACHE_LOCATION)
    if spellname+".json" in existing_spells:
        #TODO load from cache
        pass
    else:
        r = requests.get(DND_API + "spells/" + spellname)
        if r.status_code == requests.codes.ok:
            spell = r.json()
        #TODO save to cache
        return spell

def get_spell_list(spelllist):
    result = []
    for spellname in spelllist:
        spell = get_spell(spellname)
        if spell:
            result.append(spell)
        else:
            # get_spell already warns user that the spell wasn't found
            pass
    if len(spelllist) > len(result):
        sys.stderr.write(f"{len(spelllist)-len(result)} spells not found.\n")
    return result
