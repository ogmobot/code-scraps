from collections import defaultdict
import sys

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
    
def read_moon_file(lines):
    # returns {"moon name": {"mineral name": qty, "mineral name": qty}, ...}
    result = defaultdict(dict)
    moon_name = None
    for line in lines:
        if line.startswith("Moon"):
            # header
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
    if not data:
        raise ValueError("no valid moon data found")
    moon_name_len = max(len(moon) for moon in data.keys())
    lines = [" "*moon_name_len + "| R4 | R8 | R16| R32| R64|"]
    header = "="*moon_name_len + "|"
    for mineral_list in MINERAL_CATS.values():
        header += "".join(mineral[0].upper() for mineral in mineral_list)
        header += "|"
    lines.append(header)
    for moon, minerals in sorted(data.items(), key=lambda x: MOON_IDS[x[0]]):
        assert all(mineral in ALL_NAMES for mineral in minerals), f"unknown mineral in {minerals}"
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

def format_legend():
    lines = [
        f"{cat}: {', '.join(minerals)}"
        for cat, minerals in MINERAL_CATS.items()
    ]
    return "\n".join(lines)

def get_input_lines():
    lines = []
    if len(sys.argv) > 1:
        for fn in sys.argv[1:]:
            try:
                with open(fn) as fp:
                    lines.extend(fp.readlines())
            except FileNotFoundError:
                sys.stderr.write(f"File not found: {fn}\n")
    else:
        # Read lines from stdin
        sys.stderr.write("Reading from standard input. (Ctrl+D to finish.)\n")
        while True:
            try:
                line = input()
                if "." in line and "\t0." not in line: # Assume filename
                    try:
                        with open(line) as fp:
                            lines.extend(fp.readlines())
                    except FileNotFoundError:
                        sys.stderr.write(f"File not found: {line}\n")
                else:
                    lines.append(line)
            except EOFError:
                break
    return lines

if __name__ == "__main__":
    lines = get_input_lines()
    try:
        data = read_moon_file(lines)
        print(blank_unneeded_lines(format_moon_data(data)))
        print(format_legend())
    except Exception as err:
        print(f"An error occurred: {err}")
        print("Are you sure the data is in the right format?")
