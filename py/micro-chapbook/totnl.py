# Tomb of the Necro Lord
# (A Dungeon Scenario for Micro Chapbook RPG, designed and written
#  by Noah Patterson)

import random
import curses
# stdscr.addstr accepts multiple argument forms:
# - addstr(str)
# - addstr(str, attr)
# - addstr(y, x, str)
# - addstr(y, x, str, attr)

def initialise_stats(hero):
    # Adjust stats due to racial bonuses,
    # computes Health, Willpower and Gold.
    hero["stats"][{
        "human":    "ST",
        "halfling": "DE",
        "dwarf":    "WI",
        "elf":      "CH"}.get(hero["race"])] += 1

    hero["health"] = 20
    hero["health"] += hero["stats"]["ST"]
    hero["health"] += hero["stats"]["DE"]

    hero["willpower"] = 20
    hero["willpower"] += hero["stats"]["WI"]
    hero["willpower"] += hero["stats"]["CH"]

    hero["gold"] = random.randint(1, 6) + random.randint(1, 6)
    return hero

def is_proficient(hero, stat):
    return {
        "fighter":  "ST",
        "ranger":   "DE",
        "wizard":   "WI",
        "bard":     "CH"
    }.get(hero["class"]) == stat

def test_stat(hero, stat):
    # Returns True on success, or False on failure
    roll = random.randint(1, 6)
    if is_proficient(hero, stat):
        roll = min(roll, random.randint(1, 6))
    return (roll == 1) or (roll != 6 and roll <= hero["stats"][stat])

def do_menu(stdscr, opts):
    # +----+
    # |> A |
    # |  B |
    # +----+
    height = len(opts) + 2
    width = max(len(o[0]) for o in opts) + 6
    win = curses.newwin(height, width, 4, 4) # row 4, col 4
    # Draw top and bottom borders
    win.addstr(0, 0, "+" + ("-" * (width - 3)) + "+")
    win.addstr(height-1, 0, "+" + ("-" * (width - 3)) + "+")
    for index, o in enumerate(opts):
        win.addstr(1+index, 0, "|  " + o[0])
        win.addstr(1+index, width-2, "|")

    selection = 0
    pressed = None
    while True:
        for i in range(len(opts)):
            win.addstr(1+i, 1, " ")
        win.addstr(1+selection, 1, ">")
        win.refresh()
        pressed = stdscr.getch()
        if pressed == curses.KEY_UP:
            selection = (selection - 1) % len(opts)
        elif pressed == curses.KEY_DOWN:
            selection = (selection + 1) % len(opts)
        elif pressed == ord(' '):
            result = opts[selection][1]
            break
    return result

def display_character_sheet(stdscr, hero):
    lines = []
    lines.append(f"{hero['race'].title()} {hero['class'].title()}")
    lines.append("")
    for stat in ("ST", "DE", "WI", "CH"):
        lines.append(f"{stat}: {hero['stats'][stat]}")
    lines.append("")
    lines.append(f"Health:    {hero['health']}")
    lines.append(f"Willpower: {hero['willpower']}")
    lines.append(f"Gold: {hero['gold']}")
    stdscr.clear()
    for i, line in enumerate(lines):
        stdscr.addstr(1+i, 1, line)
    stdscr.refresh()
    stdscr.getch()

def do_main_menu(stdscr):
    # Returns "story", "play" or "quit"
    stdscr.clear()
    stdscr.addstr(1, 0, "TOMB OF THE NECRO LORD")
    stdscr.refresh()
    return do_menu(stdscr, [
        ("Story", "story"),
        ("Play", "play"),
        ("Quit", "quit")])

def do_story(stdscr):
    storytext = [
        "THE STORY THUS FAR: Recently, there has been a",
        "rash of bizarre killings  in the little local",
        "village where you've been staying at the Inn.",
        "The  Burgomaster  seems  totally  stumped  by",
        "these macabre slayings. There are always body",
        "parts  missing. On  top  of  that,  the  local",
        "graveyard  has  been  disturbed  and  bodies",
        "stolen.  Rumor has it that a villainous Necro",
        "Lord  has taken  up residence  in one  of the",
        "Tombs.  Can you find him and kill him?",
        "",
        "      [Press any key to continue] "]
    stdscr.clear()
    for row, text in enumerate(storytext):
        stdscr.addstr(1 + row, 2, text)
    stdscr.refresh()
    return stdscr.getkey()

def do_play(stdscr):
    hero = {}
    # TODO: let players choose stats
    # (use shop interface)
    stats = {
        "ST":1,
        "DE":1,
        "WI":1,
        "CH":1
    }
    for i in range(3):
        stats[random.choice(list(stats.keys()))] += 1
    hero["stats"] = stats

    stdscr.clear()
    stdscr.refresh()
    hero["class"] = do_menu(stdscr, [
        ("Fighter (Proficient in Strength)", "fighter"),
        ("Ranger  (Proficient in Dexterity)", "ranger"),
        ("Wizard  (Proficient in Wits)", "wizard"),
        ("Bard    (Proficient in Charisma)", "bard")])

    stdscr.clear()
    stdscr.refresh()
    hero["race"] = do_menu(stdscr, [
        ("Human    (+1 Strength)", "human"),
        ("Halfling (+1 Dexterity)", "halfling"),
        ("Dwarf    (+1 Wits)", "dwarf"),
        ("Elf      (+1 Charisma)", "elf")])
    initialise_stats(hero)
    display_character_sheet(stdscr, hero)
    # TODO: player buys starting items
    return

def main(stdscr):
    stdscr.clear()
    while True:
        option = do_main_menu(stdscr)
        if option == "story":
            do_story(stdscr)
        elif option == "play":
            do_play(stdscr)
        elif option == "quit":
            break
        else:
            break
    return

if __name__=="__main__":
    curses.wrapper(main)
