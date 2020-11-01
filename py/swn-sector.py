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
            hexmap[(w+1, h+1)] = None
    # hexes may contain:
    # - None (no stars in hex)
    # - {} (unpopulated star in hex)
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
        rand_x = d(8)
        rand_y = d(10)
        if hexmap[(rand_x, rand_y)] == None:
            hexmap[(rand_x, rand_y)] = {}
            pass
        else:
            # place star in adjacent hex (otherwise skip)
            valid_neighbours = list(filter(
                            (lambda coord: is_valid_hex(hexmap, coord[0], coord[1])), 
                            adjacent_hexes(rand_x, rand_y)))
            if valid_neighbours:
                coord = random.choice(valid_neighbours)
                hexmap[(coord[0], coord[1])] = {}
    return

def make_random_primary_worlds(hexmap):
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
