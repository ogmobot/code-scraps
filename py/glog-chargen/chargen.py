#!/usr/bin/python3

import csv
import random

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

def print_random_race():
    csv_files = [
        # first listed is default
        "races-iron-and-ink.csv",
        "races-goblins-in-a-trenchcoat.csv",
    ]
    #TODO get directory listing with import os, or an external settings file?
    race_tables = {filename:get_rows(filename) for filename in csv_files}
    default_race_table = race_tables[csv_files[0]]
    race = roll_for_race(default_race_table)
    for key, value in race.items():
        print("{}:\t{}".format(key, value))
    return race
