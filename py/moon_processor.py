from collections import defaultdict

MINERAL_CATS = {
     "R4": ["Bitumens",  "Coesite",   "Sylvite",    "Zeolites"],
     "R8": ["Cobaltite", "Euxenite",  "Scheelite",  "Titanite"],
    "R16": ["Chromite",  "Otavite",   "Sperrylite", "Vanadinite"],
    "R32": ["Carnotite", "Cinnabar",  "Pollucite",  "Zircon"],
    "R64": ["Loparite",  "Monazite",  "Xenotime",   "Ytterbite"]
}
ALL_NAMES = set()
for m_types in MINERAL_CATS.values():
    ALL_NAMES = ALL_NAMES.union(m_types)
MOON_IDS = dict()
    
def read_moon_file(fp):
    # returns {"moon name": {"id": int, "mineral name": qty, "mineral name": qty}, ...}
    result = defaultdict(dict)
    moon_name = None
    for line in fp:
        if line.startswith("Moon"):
            continue
        elif line.startswith("\t"):
            # data line
            parts = line.strip().split("\t")
            result[moon_name][parts[0]] = float(parts[1])
            MOON_IDS[moon_name] = int(parts[5])
            # ignore Ore TypeID, SolarSystemID, PlanetID
        else:
            # moon title
            moon_name = line.strip()
    return result

def format_moon_data(data):
    moon_name_len = max(len(moon) for moon in data.keys())
    lines = [" "*moon_name_len + "| R4 | R8 | R16| R32| R64|"]
    header = "="*moon_name_len + "|"
    for mineral_list in MINERAL_CATS.values():
        header += "".join(mineral[0].upper() for mineral in mineral_list)
        header += "|"
    lines.append(header)
    for moon, minerals in sorted(data.items(), key=lambda x: MOON_IDS[x[0]]):
        assert all(mineral in ALL_NAMES for mineral in minerals), minerals
        row_string = moon
        while len(row_string) < moon_name_len:
            row_string += " "
        row_string += "|"
        for category, mineral_types in MINERAL_CATS.items():
            col_string = ""
            for mineral_type in mineral_types:
                if mineral_type in minerals:
                    out_digit = str(round(minerals[mineral_type] * 10))
                    if len(out_digit) > 1:
                        out_digit = "#"
                    col_string += out_digit
                else:
                    col_string += "-"
            col_string += "|"
            row_string += col_string
        lines.append(row_string)
    lines.append("="*moon_name_len + "==========================")
    return "\n".join(lines)

def blank_unneeded_lines(table):
    # Turns |1234|----|1234|----| into |1234|----|1234|    |
    ncols = 4
    colwidth = 4
    from_string = ("-"*colwidth +"|")*ncols + "\n"
    to_string   = from_string.replace("-", " ")
    for _ in range(ncols):
        table = table.replace(from_string, to_string)
        from_string = from_string[colwidth + 1:]
        to_string   = to_string[colwidth + 1:]
    return table

data = defaultdict(dict)
while True:
    fn = input("Filename: ")
    if not fn:
        break
    try:
        with open(fn) as fp:
            new_data = read_moon_file(fp)
        for moon, minerals in new_data.items():
            data[moon].update(minerals)
    except FileNotFoundError:
        print("File not found.")

print(blank_unneeded_lines(format_moon_data(data)))
