import json
import random

tables = {}

def load_tables(filename):
    ts = {}
    with open(filename, "r") as f:
        ts = json.load(f)
    return ts

def make_character():
    c = {}
    c["NOTES"] = []
    c["SPELL SLOTS"] = 0
    c["SPELLS"] = []
    c["PATHS"] = []
    c["ATTACK"] = 0
    c["ARMOR"] = 6
    c["BACKPACK"] = []
    c["WORN"] = []
    c["BELT"] = []
    c["HANDS"] = []
    # Roll or choose abilities
    a = random.choice(tables["abilities"]["abilities"])
    c["STR"], c["DEX"], c["WIL"] = a
    # Record maximum health
    c["MAX HEALTH"] = 4
    c["HEALTH"] = 4
    # Choose starting feature
    f = random.choice(tables["abilities"]["features"])
    apply_bonus(c, f)
    # Choose six items
    for i in range(6):
        # Duplicates are fine; every item takes a different slot.
        c["BACKPACK"].append(random.choice(tables["characters"]["items"]))
    # Choose combat gear
    grant_armor(c, "light armor")
    grant_armor(c, "shield")
    for i in range(2):
        category, weapons = random.choice(
            list(tables["abilities"]["combat gear"].items()))
        w = random.choice(weapons)
        w_desc = f"{w} ({category} weapon)"
        c["BACKPACK"].append(w_desc)
    # Choose background and appearance
    for (table_name, caption) in [
        ("appearances",      "Appearance:      "),
        ("physical details", "Physical detail: "),
        ("backgrounds",      "Background:      "),
        ("personalities",    "Personality:     "),
        ("mannerisms",       "Mannerism:       ")
    ]:
        var = random.choice(tables["characters"][table_name])
        c["NOTES"].append(f"{caption}{cfl(var)}")
    # Clothes are always worn, sometimes under armor
    clothing = random.choice(tables["characters"]["clothing"])
    c["WORN"].append(f"Clothes ({clothing})")
    # Set up spells
    while len(c["SPELLS"]) < c["SPELL SLOTS"]:
        c["SPELLS"].append(make_random_spell())
    # Record name, level and XP
    c["NAME"] = make_random_name()
    c["XP"] = 0
    c["LEVEL"] = 1
    return c

def cfl(s):
    # Capitalize first letter only
    if s:
        s = s[0].upper() + s[1:]
    return s

def apply_bonus(character, feature):
    # Returns whether feature was successfully applied
    tokens = feature.split()
    if tokens[0] == "bonus":
        size = int(tokens[1])
        stat = " ".join(tokens[2:])
        character[stat] += size
        return True
    elif tokens[0] == "path":
        path = random.choice(list(tables["abilities"]["paths"].items()))
        path_description = f"{path[0].title()}: advantage on danger rolls related to {', '.join(path[1])}"
        character["PATHS"].append(path[0])
        character["NOTES"].append(path_description)
        return True
    else:
        return False

def grant_armor(character, armor):
    new_armor = tables["abilities"]["armor"][armor]
    current_armor = None
    for worn_item in character[new_armor["slot"]]:
        if worn_item in tables["abilities"]["armor"]:
            current_armor = tables["abilities"]["armor"][worn_item]
            break
    if not current_armor:
        # Put on armor
        character[new_armor["slot"]].append(armor)
        character["ARMOR"] += new_armor["bonus"]
        return True
    elif new_armor["bonus"] > current_armor["bonus"]:
        # Swap armor
        character[new_armor["slot"]].remove(worn_item)
        character["ARMOR"] -= current_armor["bonus"]
        character["BACKPACK"].append(worn_item)
        character[new_armor["slot"]].append(armor)
        character["ARMOR"] += new_armor["bonus"]
        return True
    else:
        # Stash armor
        character["BACKPACK"].append(armor)
        return True

def make_random_name():
    gender = random.choice(["male", "female"])
    firstname = random.choice(tables["characters"][f"{gender} names"])
    society = random.choice(["upper class", "lower class"])
    if society == "upper class" and random.randint(1, 20) == 1:
        # "This table can also be used for upper-class first names,
        #  if you want them to sound extra snobby."
        firstname = random.choice(tables["characters"]["upper class surnames"])
    surname = random.choice(tables["characters"][f"{society} surnames"])
    return f"{firstname} {surname}".title()

def make_random_spell():
    category = random.choice(tables["magic"]["categories"])
    first_table, second_table = category.split(", ")
    first_word = random.choice(tables["magic"][first_table])
    second_word = random.choice(tables["magic"][second_table])
    return f"{first_word} {second_word}".title()

def main():
    global tables
    tables["characters"] = load_tables("./tables-characters.json")
    tables["abilities"] = load_tables("./tables-abilities.json")
    tables["magic"] = load_tables("./tables-magic.json")

if __name__ == "__main__":
    main()
