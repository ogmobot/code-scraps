# This version is designed to work on spells
# that were copy-pasted from 5e.d20srd.org.

# Example interaction
#
# Paste spell description below.
## Faerie Fire
## 1st-level evocation
## Casting Time:	1 action
## Range:	60 feet
## Components:	V
## Duration:	Concentration, up to 1 minute
## Each object in a 20-foot cube within range is ...
## 
## Any attack roll against an affected creature ...
#
# Copy and paste the following LaTeX code into your document.
# ===
# \begin{spellbox}{Augury}
# \noindent
# {\setlength{\parindent}{0em}\textit{2nd-level divination (ritual)}}\\
# \hline
# \spellbasics[
#   castingtime={1 minute},
#   range={Self},
#   components={V, S, M (specially marked sticks worth 25 gp)},
#   duration={Instantaneous}
#   ]
#   \hline
# You receive an omen from an otherworldy entity about the results of a specific course of action that you plan to take within the next 30 minutes.
# 
# The DM makes the roll in secret.
# 
# \begin{spellaction}[At Higher Levels]
# Succeed more often.
# \end{spellaction}
# ===
import re, sys
DEBUG = False

def build_spell_from_string(input_string):
    input_lines = input_string.strip().split('\n',2)
    name = input_lines[0].strip()
    print("Processing {}...".format(name))
    output_spell = Spell(name)
    output_spell.school = input_lines[1].strip()
    remainder = input_lines[2]
    while remainder:
        m = re.match(r'^([^\:\n]*)\:([^\:\n]*\([^\:\)]*\))\n',remainder)
        n = False
        if not m:
            m = re.match(r'^([^\:\n]*)\:([^\:\n]*)\n',remainder)
        if not m: # search for extras, e.g. "At Higher Levels."
            n = re.match(r'\n([A-Z][a-z]*( [A-Z][a-z]*)*)\. ([^\n]*)',remainder)
        if m and m.group(1).lower() in ["casting time","range","components","duration"]:
            # add attribute
            output_spell.add_attrib(m.group(1).title(),m.group(2).strip())
            remainder = remainder[:m.start()] + remainder[m.end():]
        elif n:
            # add extra
            output_spell.add_extra(n.group(1),n.group(3).strip())
            remainder = remainder[:n.start()] + remainder[n.end():]
        else:
            if '\n' in remainder:
                line,remainder = remainder.split('\n',1)
            else:
                line = remainder
                remainder = ''
            # add description
            if not line:
                output_spell.description += "\n\n"
            else:
                output_spell.description += line.strip()
    # todo: add extras
    print("Done.")
    return output_spell

class Spell:
    def __init__(self,name):
        self.name = name
        self.attribs = {} # e.g. Casting Time, Range, etc.
        self.extras = {} # e.g. At Higher Levels., etc.
        self.description = ""
        self.school = "" # level and school
    def add_attrib(self,key,value):
        self.attribs[key] = value
    def add_extra(self,key,value):
        self.extras[key] = value
    def __repr__(self):
        return "Spell Object: {}".format(self.name)
    def __str__(self):
        return self.build_latex_string()
    
    def build_latex_string(self):
        output = ""
        output += "\\begin{{spellbox}}{{{0}}}\n\n".format(self.name) # \begin{spellbox}{Augury}
        output += "{{\\setlength{{\\parindent}}{{0em}}\\textit{{{0}}}}}\\\\\n".format(self.school) # {\setlength{\parindent}{0em}\textit{2nd-level divination (ritual)}}\\
        output += "\\hline\n\\spellbasics[\n"
        
        for attrib in ["Casting Time","Range","Components","Duration"]:
            if attrib.lower() in [x.lower() for x in self.attribs]:
                output += "\t{0}={{{1}}},\n".format(attrib.replace(' ','').lower(),self.attribs[attrib]) # Such a hack T_T
        if output[-2]==',':
            output = output[:-2]+'\n' # Such a DIRTY HACK T____T
        output += "]\n\\hline\n"
        # Extra attributes for spells -- probably not needed.
        # for attrib in sorted(self.attribs.keys()):
        #     if attrib not in ["Casting Time","Range","Components","Duration"]:
        #         output += "\\textbf{{{0}:\\tab}}{1}\n\n".format(attrib,self.attribs[attrib])
        output += self.description
        output += "\n\n"
        for attrib in sorted(self.extras.keys()):
            output += "\n\n\\begin{{spellaction}}[{0}]\n".format(attrib)
            output += self.extras[attrib]
            output += "\n\\end{spellaction}\n"
        output += "\\end{spellbox}"
        return output

def mainloop():
    while True:
        print("Paste or type spell data below, then type ^D. Leave blank to quit.")
        text = ""
        while True:
            try:
                line = input()
            except EOFError:
                break
            text += line + '\n'
        if not text:
            break
        #try:
        if True:
            out_spell = build_spell_from_string(text)
            out_text = str(out_spell)
            print("Paste the following text into the LaTeX document.")
            print()
            print(out_text)
            print()
        #except Exception as err:
        else:
            print("There was an error in interpreting the pasted data.")
            print("{}".format(err))
    return

if __name__=="__main__":
    if not DEBUG:
        # run in batch mode
        f = input("Enter input filename: ")
        if f:
            f_input = open(f,"rU")
        else:
            f_input = sys.stdin
        all_spells = f_input.read().strip().split("===")
        num_spells = len(all_spells)
        f_input.close()
        spell_array = [str(build_spell_from_string(text)) for text in all_spells]
        f = input("Enter output filename: ")
        if f:
            f_output = open(f,"w")
        else:
            f_output = sys.stdout
        f_output.write("\n\n".join(spell_array))
        if f_output != sys.stdout: # -_-
            f_output.close()
        print("Successfully wrote {} spell(s) to output file.".format(num_spells))
        
    elif DEBUG:
        mainloop()
    else:
        #x = Spell("Fly")
        #x.school = "3rd-level transmutation"
        #x.attribs["Casting Time"] = "1 action"
        #x.attribs["Range"] = "Touch"
        #x.attribs["Components"] = "V, S, M (a wing feather from any bird)"
        #x.attribs["Duration"] = "Concentration, up to 10 minutes"
        #x.description = "You touch a willing creature. The target gains a flying speed of 60 feet for the duration. When the spell ends, the target falls if it is still aloft, unless it can stop the fall."
        #x.extras["At Higher Levels"] = "When you cast this spell using a spell slot of 4th level or higher, you can target one additional creature for each slot level above 3rd."
        #print(x)
        y = build_spell_from_string("""Fly
3rd-level transmutation
Casting Time:	1 action
Range:	Touch
Components:	V, S, M (a wing feather from any bird)
Duration:	Concentration, up to 10 minutes
You touch a willing creature. The target gains a flying speed of 60 feet for the duration. When the spell ends, the target falls if it is still aloft, unless it can stop the fall.

At Higher Levels. When you cast this spell using a spell slot of 4th level or higher, you can target one additional creature for each slot level above 3rd.""")
        print(y)
