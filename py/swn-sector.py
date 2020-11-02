import random

def d(n):
    return random.randint(1, n)

def adjacent_hexes(x, y):
    # returns a list of (x, y) tuples adjacent to the given hex
    result = [
            (x, y-1),
    (x-1, y),       (x+1, y),
            (x, y+1)
    ]
    if x%2 == 0:
        # even column
        result.extend([(x-1, y+1), (x+1, y+1)])
    else:
        # odd column
        result.extend([(x-1, y-1), (x+1, y-1)])
    return result

def is_valid_hex(hexmap, x, y):
    # this function must be updated if hexmap gets non-tuple keys
    coords = list(hexmap.keys())
    max_x = max(c[0] for c in coords)
    max_y = max(c[1] for c in coords)
    return (x >= 1 and x <= max_x) and (y >= 1 and y <= max_y)

def make_hexmap(width, height):
    # (hexes indexed from 1)
    hexmap = {}
    for x in range(width):
        for y in range(height):
            hexmap[(x+1, y+1)] = None
    # hexes may contain, for example:
    # - None (i.e. no system)
    # - {"name":"Sol", "planets":[]}
    # - {"name":"Sol", "planets":[{"name":"Sol I", "atmosphere":"None"}]}
    return hexmap

# 1,1     3,1     ...
#     2,1     4,1 ...
# 1,2     3,2     ...
#     2,2     4,2 ...
# 1,3     3,3     ...
#     2,3     4,3 ...
# 1,4     3,4     ...

def make_random_stars(hexmap):
    total_stars = d(10) + 20
    for star in range(total_stars):
        new_star = {
            "planets": []
        }
        rand_x = d(8)
        rand_y = d(10)
        if hexmap[(rand_x, rand_y)] == None:
            # place star
            hexmap[(rand_x, rand_y)] = new_star
            pass
        else:
            # place star in adjacent hex (otherwise skip)
            valid_neighbours = list(filter(
                            (lambda coord: is_valid_hex(hexmap, coord[0], coord[1])), 
                            adjacent_hexes(rand_x, rand_y)))
            if valid_neighbours:
                coord = random.choice(valid_neighbours)
                # place star
                hexmap[(coord[0], coord[1])] = new_star
    return

def setup_planet(planet, prop):
    if prop == "tags":
        planet["tags"] = random.sample([
            "Abandoned Colony",     "Flying Cities",        "Misandry/Misogyny",    "Rigid Culture",
            "Alien Ruins",          "Forbidden Tech",       "Night World",          "Rising Hegemon",
            "Altered Humanity",     "Former Warriors",      "Nomads",               "Ritual Combat",
            "Anarchists",           "Freak Geology",        "Oceanic World",        "Robots",
            "Anthropomorphs",       "Freak Weather",        "Out of Contact",       "Seagoing Cities",
            "Area 51",              "Friendly Foe",         "Outpost World",        "Sealed Menace",
            "Badlands World",       "Gold Rush",            "Perimeter Agency",     "Secret Masters",
            "Battleground",         "Great Work",           "Pilgrimage Site",      "Secretarians",
            "Beastmasters",         "Hatred",               "Pleasure World",       "Seismic Instability",
            "Bubble Cities",        "Heavy Industry",       "Police State",         "Shackled World",
            "Cheap Life",           "Heavy Mining",         "Post-Scarcity",        "Societal Despair",
            "Civil War",            "Hivemind",             "Preceptor Archive",    "Sole Supplier",
            "Cold War",             "Holy War",             "Pretech Cultists",     "Taboo Treasure",
            "Colonized Population", "Hostile Biosphere",    "Primitive Aliens",     "Terraform Failure",
            "Cultural Power",       "Hostile Space",        "Prison Planet",        "Theocracy",
            "Cybercommunists",      "Immortals",            "Psionics Academy",     "Tomb World",
            "Cyborgs",              "Local Specialty",      "Psionics Fear",        "Trade Hub",
            "Cyclical Doom",        "Local Tech",           "Psonics Worship",      "Tyranny",
            "Desert World",         "Major Spaceyard",      "Quarantined World",    "Unbraked AI",
            "Doomed World",         "Mandarinate",          "Radioactive World",    "Urbanized Surface",
            "Dying Race",           "Mandate Base",         "Refugees",             "Utopia",
            "Eugenic Cult",         "Maneaters",            "Regional Hegemon",     "Warlords",
            "Exchange Consulate",   "Megacorps",            "Restrictive Laws",     "Xenophiles",
            "Fallen Hegemon",       "Mercenaries",          "Revanchists",          "Xenophobes",
            "Feral World",          "Minimal Contact",      "Revolutionaries",      "Zombies"
        ], 2)
        return
    elif prop == "atmosphere":
        return
    elif prop == "temperature":
        return
    elif prop == "biosphere":
        return
    elif prop == "population":
        return
    elif prop == "tech level":
        return
    else:
        raise ValueError(f"Tried to set unknown planet prop ({prop})")

def make_planet():
    planet = {}
    setup_planet(planet, "tags")
    setup_planet(planet, "atmosphere")
    setup_planet(planet, "temperature")
    setup_planet(planet, "biosphere")
    setup_planet(planet, "population")
    setup_planet(planet, "tech level")
    # TODO name
    return planet

def make_random_primary_worlds(hexmap):
    # put at least one planet in each system
    for coord, star in hexmap.items():
        if star == None:
            continue
        planet = make_planet()
        star["planets"].append(planet)
    return

def make_trade_routes(hexmap):
    return

def make_sector_map():
    sector_map = make_hexmap(8, 10)
    make_random_stars(sector_map)
    make_random_primary_worlds(sector_map)
    make_trade_routes(sector_map)
    # out of scope:
    # build out the important worlds
    # choose factions and relations
    return sector_map

def grid_coords(x, y):
    # translates a particular hex coord into a (row, col) tuple for a grid
    # (note that row represents *y* position, not x)
    # e.g. (1, 1) => (0, 0)
    #      (2, 2) => (3, 1)
    #      (3, 3) => (4, 2)
    #      (4, 4) => (7, 3)
    return ((2*y) - (x%2) - 1, x-1) #don't touch

def star_desc(stardict):
    # returns a string representation of a dict representing a star
    return repr(stardict)

def print_sector_map(hexmap):
    # Displays a keyed hex map
    # Display map
    # [  ]    [  ]    ...
    #     [01]    [  ]...
    # [  ]    [  ]    ...
    #     [02]    [03]...
    # [  ]    [04]    ...
    #     [  ]    [  ]...
    # [05]    [  ]    ...
    GAP = "    "
    stars = [(key, val) for key, val in hexmap.items() if val != None]
    stars.sort(key=(lambda x:grid_coords(*(x[0]))))
    print_grid = {}
    for index, star in enumerate(stars):
        print_grid[grid_coords(*star[0])] = f"{index + 1:02}"
    for row in range(20):
        print_row = ""
        for col in range(8):
            if row%2 == col%2:
                print_row += "[{:2}]".format(print_grid.get((row, col), ""))
            else:
                print_row += GAP
        print(print_row)
    # Display key
    for index, star in enumerate(stars):
        # Already sorted -- don't change order!
        print(f"{index + 1:02}: {star_desc(star[1])}")
    return
