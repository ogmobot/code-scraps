# This version is designed to work on spells
# that were copy-pasted from d20pfsrd.

import sys
DEBUG = False
# Create a list of attributes so they can be detected and ordered appropriately
ATTRIBUTES = [
    "casting time",
    "components",
    "component", # this might just be a typo on Detect Magic, though...
    "range",
    "targets",
    "target", # prestidigitation has "target, effect, or area"
    "effect",
    "spread",
    "area",
    "line of effect",
    "duration",
    "saving throw",
    "spell resistance"
]

def build_spell_from_string(input_string):
    input_lines = input_string.strip().split("\n")
    name = input_lines.pop(0).strip()
    if name.isupper():
        name = name.title()
    sys.stderr.write(f"Processing {name}...\n")
    output_spell = Spell(name)
    write_to_description = False
    while len(input_lines) > 0:
        line = input_lines.pop(0)
        if line.strip() == "DESCRIPTION":
            write_to_description = True
        elif write_to_description:
            if line.strip() == "":
                output_spell.description += "\n\n"
            else:
                output_spell.description += line
        elif line.strip() == "":
            continue
        elif line.strip() == "CASTING" or line.strip() == "EFFECT":
            continue
        elif line.lower().startswith("school"):
            schooltext, _ = line.split(";", 1)
            output_spell.school = schooltext.split(" ",1)[1].title()
        elif (line.lower().startswith("saving throw")) and (";" in line):
            savthrow, spellres = line.split(";", 1)
            input_lines.insert(0, savthrow.strip())
            input_lines.insert(1, spellres.strip())
        else:
            for a in ATTRIBUTES:
                if line.lower().startswith(a):
                    output_spell.add_attrib(a, line[len(a):].strip())
                    if a == "spell resistance" and ("DESCRIPTION" not in [l.strip() for l in input_lines]):
                        write_to_description = True
                    break
    sys.stderr.write("Done.\n")
    return output_spell

class Spell:
    def __init__(self,name):
        self.name = name
        self.school = "" # e.g. Evocation [Light]
        self.attribs = {} # e.g. {"Casting Time": "1 standard action"}
        self.description = ""
    def add_attrib(self,key,value):
        self.attribs[key] = value
    def __repr__(self):
        return "Spell Object: {}".format(self.name)
    def __str__(self):
        return self.build_latex_string()
    
    def build_latex_string(self):
        output_sections = []
        output_separator = "\n\\hline\n"
        output_sections.append(self.build_spell_header().strip())
        output_sections.append(self.build_spell_attributes().strip())
        output_sections.append(self.build_spell_description().strip())
        return output_separator.join(output_sections)

    def build_spell_header(self):
        output = ""
        output += f"\\begin{{spellbox}}{{{self.name}}}\n\n" # \begin{spellbox}{Augury}
        output += f"{{\\setlength{{\\parindent}}{{0em}}\\textit{{{self.school}}}}}\\\\\n" # {\setlength{\parindent}{0em}\textit{Transmutation}}\\
        return output

    def build_spell_attributes(self):
        output = "{\\color{black}\n"
        for k in sorted(self.attribs.keys(), key=lambda k: (ATTRIBUTES.index(k.lower()) if k.lower() in ATTRIBUTES else len(ATTRIBUTES))):
            output += r"    \textbf{"
            output += k.title()
            output += r":} "
            output += self.attribs.get(k,"")
            output += "\\\\\n"
        output += "}"
        return output

    def build_spell_description(self):
        return self.description + "\n\n\\end{spellbox}"


def mainloop(f_input, f_output):
    all_spells = f_input.read().strip().split("===")
    num_spells = len(all_spells)
    spell_array = [str(build_spell_from_string(text)) for text in all_spells]
    f_output.write("\n\n".join(spell_array))
    f_output.close()
    sys.stderr.write(f"Successfully wrote {num_spells} spell(s) to output file.\n")
    return

if __name__=="__main__":
    if not DEBUG:
        f = input("Enter input filename: ")
        g = input("Enter output filename: ")
        f_input = open(f, "rU") if f else sys.stdin
        f_output = open(g, "w") if g else sys.stdout
        mainloop(f_input, f_output)
    else:
        x = build_spell_from_string("""Protection from Chaos

School abjuration [lawful]; Level bloodrager 1, cleric/oracle 1, inquisitor 1, paladin 1, shaman 1, sorcerer/wizard 1, summoner/unchained summoner 1; Domain law 1; Subdomain purity 1

CASTING

Casting Time 1 standard action
Components V, S, M/DF

EFFECT

Range touch
Target creature touched
Duration 1 min./level (D)
Saving Throw Will negates (harmless); Spell Resistance no; see text

DESCRIPTION

This spell functions like protection from evil, except that the deflection and resistance bonuses apply to attacks made by chaotic creatures. The target receives a new saving throw against control by chaotic creatures and chaotic summoned creatures cannot touch the target.""")
        print(x)
