#!/usr/bin/python3

# Character creation method taken from Skerples' "Many Rats on Sticks"

import csv
import random
from collections import OrderedDict

SHEET_WIDTH = 80
SHEET_HEIGHT = 24

d = lambda n:random.randint(1, n)

def die_parse(s):
    # Given a string s, e.g. "1d20+3", make that roll.
    # Use e.g. 1d6+-3 to subtract rolls.
    parts = [x.strip() for x in s.split("+")]
    total = 0
    for p in parts:
        if p[0] == "d":
            # e.g. d60
            total += d(int(p[1:]))
        elif "d" in p:
            # e.g. 1d60
            num_dice, die_size = [int(x) for x in p.split("d")]
            subtotal = 0
            for i in range(num_dice):
                subtotal += d(die_size)
            total += subtotal
        else:
            # e.g. 60
            total += int(p)
    return total

def get_rows(filename):
    with open(filename, "r") as inputfile:
        reader = csv.DictReader(inputfile)
        return [row for row in reader]

def roll_for_race(race_table):
    # "Choose or roll for your Race. Your character's Race will grant them
    # a Perk, a Drawback, and one Stat they can reroll."
    # determine what roll to make
    roll_string = list(race_table[0].keys())[0]
    die_roll = die_parse(roll_string)
    # search first column of file for matching range
    for row in race_table:
        key = row[roll_string]
        if "-" in key:
            # range such as "1-10: Human"
            lowerbound, upperbound = [int(x) for x in key.split("-")]
            if die_roll >= lowerbound and die_roll <= upperbound:
                return row
        else:
            # literal such as "11: Elf"
            if int(key) == die_roll:
                return row
    # this should never happen, but might for a table like:
    # +-----+-----------+
    # | d6  | Result    |
    # | 1-5 | Something |
    # +-----+-----------+
    print("Warning: rolled {} but couldn't find a matching entry in the table!".format(die_roll))
    return race_table[0]

def roll_for_stats(reroll):
    # "Roll your Stats. 3d6 in order. You may reroll the stat given by your
    # Race and pick the higher result.
    result = OrderedDict()
    for stat in ["STR", "DEX", "CON", "INT", "WIS", "CHA"]:
        result[stat] = die_parse("3d6")
        if stat.upper() == reroll.upper():
            result[stat] = max(result.get(stat), die_parse("3d6"))
    return result

def stat_bonus(val):
    return min((val//3)-3, 5)

def armor_bonus(name):
    name = name.lower()
    if "leather" in name:
        return 2
    elif "chain" in name:
        return 4
    elif "plate" in name:
        return 6
    elif "shield" in name:
        return 1
    else:
        raise ValueError("bad armor type ({})".format(name))

def derived_stats(character):
    result = OrderedDict()
    # Attack is based only on level
    result["Attack"] = {
        0:11,
        1:11,
        2:12,
        3:12,
        4:13,
        5:13,
        6:14,
        7:14
    }.get(character.get("Level", 0))
    # Defense is based on either DEX or armor
    armors = set(item.lower() for item in character["Inventory"] if "armor" in item)
    has_shield = any(["shield" in item.lower() for item in character["Inventory"])
    # Might get fooled by e.g. "Amulet of Shielding"... but that's ok for now
    result["Defense"] = 10 + max(
        stat_bonus(character.get("Stats",{}).get("DEX",0)),
        max(armor_bonus(a) for a in armors)
    )
    pass

def print_character(character):
    summary_string = "Level {} {} {} {} ({})".format(
        character.get("Level", 0),
        character.get("Gender", "Neuter"),
        character.get("Race", {}).get("Name", "[Race]"),
        character.get("Class", {}).get("Name", "[Class]"),
        character.get("Background", {}).get("Name", "[Background]")
    )
    stat_col = character["Stats"]
    derived_col = derived_stats(character)

### Example character sheet (to scale) ###
"""
0         1         2         3         4         5         6         7
01234567890123456789012345678901234567890123456789012345678901234567890123456789
+------------------------------------------------------------------------------+0
|                                                                              |
| JOE BLOGGS                                                                   |
|                                                                              |
| Level 1 Male Human Fighter (Soldier)                                         |
|                                                                              |
| 1 Boon to benefits from beneficial magic.                                    |
| 1 Bane on Saves vs mutation.                                                 |
| You've served across the seas and over the mountains.                        |8
| Make up 1d6 ludicrous lies and gain the "Foreign Parts" skill.               |
|                                                                              |
| Stats:     Attack:   11     Class abilities:     Equipment:                  |
| STR 15     Defense:  10     Parry                Leather armor               |
| DEX 10     Movement: 12                          Sword                       |
| CON 11     Stealth:   5                          Bow                         |
| INT  6     Save:      5                          20 arrows                   |
| WIS  8     Max HP:    7                          Blanket                     |16
| CHA  8                                           3 rations                   |
|                                                  2 cp                        |
|                                                                              |
|                                                                              |
|                                                                              |
| *Before playing, you may reroll one stat. Keep the higher result.            |
+------------------------------------------------------------------------------+
"""

def make_random_character():
    character = {
        "Race": None,
        "Class": None,
        "Stats": None,
        "Setup": []
    }

    # Race
    csv_files = [
        # first listed is default
        "races-iron-and-ink.csv",
        "races-goblins-in-a-trenchcoat.csv",
    ]
    #TODO get directory listing with import os, or an external settings file?
    # Either way, player needs to be able to choose which race table to use.
    race_tables = {filename:get_rows(filename) for filename in csv_files}
    default_race_table = race_tables[csv_files[0]]
    race = roll_for_race(default_race_table)
    #for key, value in race.items():
        #print("{}:\t{}".format(key, value))
    character["Race"] = race

    # Stats
    character["Stats"] = roll_for_stats(character["Race"]["Reroll"])
    if character["Race"]["Reroll"].lower() == "choice":
        character["Setup"].append("Reroll one stat of your choice.")
