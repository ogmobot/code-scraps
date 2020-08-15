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
        w_desc = f"{w} ({category})"
        grant_weapon(c, w_desc)
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
    c["WORN"].append(f"clothes ({clothing})")
    # Set up spells
    while len(c["SPELLS"]) < c["SPELL SLOTS"]:
        c["SPELLS"].append(make_random_spell())
    # Record name, level and XP
    c["NAME"] = make_random_name()
    c["XP"] = 0
    c["LEVEL"] = 1
    return c

def format_character_sheet(c):
    left_panel = f"""
{c["NAME"]} (LVL {c["LEVEL"]} / {c["XP"]} XP)
STR +{c["STR"]}  ATTACK +{c["ATTACK"]}
DEX +{c["DEX"]}  ARMOR  {"{:2}".format(c["ARMOR"])}
WIL +{c["WIL"]}  HEALTH {"{:2}".format(c["HEALTH"])} ({c["MAX HEALTH"]})
"""
    for spell in c["SPELLS"]:
        left_panel += f"SPELL: {spell}\n"
    notes_panel = "\n".join(c["NOTES"])
    equipment_panel = "\n".join([
        "HANDS: " + ", ".join(c["HANDS"]),
        "BELT:  " + ", ".join(c["BELT"]),
        "WORN:  " + ", ".join(c["WORN"]),
        "BACKPACK:\n" + "\n".join(c["BACKPACK"])
    ])
    return ("\n===\n".join([
        left_panel.strip(),
        notes_panel.strip(),
        equipment_panel.strip()]))

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
        path_description = f"{path[0].title()} (advantage on danger rolls related to {', '.join(path[1])})"
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

def grant_weapon(character, weapon_desc):
    # This will necessarily have a different structure to grant_armor,
    # because we need to check what category weapon_desc is in.
    weapon, category = weapon_desc[:-1].split(" (")
    weapon_data = tables["abilities"]["weapon categories"][category]
    if not character["HANDS"]:
        character["HANDS"].append(weapon_desc)
        return True
    elif character["HANDS"] == ["shield"]:
        if weapon_data["hands"] == 2:
            # 2H weapon replaces shield
            unequip(character, "shield")
            character["HANDS"].append(weapon_desc)
            return True
        elif weapon_data["hands"] == 1:
            # 1H weapon complements shield
            character["HANDS"].append(weapon_desc)
            return True
    # Try the belt, if there's room
    elif len(character["BELT"]) < 2:
        character["BELT"].append(weapon_desc)
        return True
    else:
        character["BACKPACK"].append(weapon_desc)
        return True

def unequip(character, item):
    # Moves the given item from hands to belt or backpack
    if item not in character["HANDS"]:
        return False
    else:
        character["HANDS"].remove(item)
        if item == "shield":
            # Right now, "shield" is the only object that improves armor
            # when held in hands
            character["ARMOR"] -= 1
        if len(character["BELT"]) < 2:
            character["BELT"].append(item)
            return True
        else:
            character["BACKPACK"].append(item)
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
