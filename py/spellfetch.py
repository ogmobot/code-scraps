#!/bin/python3

import os       # to access the cache location
import sys      # for access to stdin, stdout and stderr
import requests # for HTTPS requests to the API
import json     # for dumping and loading JSON
import argparse # for command line arguments
import html     # for escape characters in format_html
CACHE_LOCATION = os.environ["HOME"] + "/.spellfetch/"
DND_API = "https://www.dnd5eapi.co/api/"

# Note that this won't be able to find spells from e.g. Elemental Evil
# Player's Companion, although those may be added manually by placing a
# JSON file into the cache.

# Generic utility functions
def remove_personal_name(spellname):
    # Converts "Mordekainen's Magnificent Mansion" to "Magnificent Mansion"
    if spellname.lower() == "mordenkainen's sword":
        return "arcane sword"
    elif spellname.lower() == "nystul's magic aura":
        return "arcanist's magic aura"
    elif spellname.lower() == "bigby's hand":
        return "arcane hand"
    elif spellname.lower() == "jim's magic missile":
        # Doesn't appear in API; take a guess
        return "arcanist's magic missile"
    elif spellname.lower() == "hunter's mark":
        return spellname
    elif spellname.lower() == "heroes' feast":
        # Heroes' Feast doesn't end with "'s" anyway
        return spellname
    words = spellname.split()
    if words and words[0].endswith("'s"):
        words.pop(0)
    return " ".join(words)

def clean_string(s):
    # Converts a string to lowercase, removes apostrophes, and
    # replaces non-alnum chars with '-'
    return "".join((c if c.isalnum() else '-') for c in s.lower() if c != "'").strip("-")

def file_to_list(fp):
    return [line.strip() for line in fp if line.strip()]

# API request functions
def get_spell(spellname):
    cleanname = clean_string(spellname)
    cleanname_generic = clean_string(remove_personal_name(spellname))
    spell = None
    existing_spells = []
    try:
        existing_spells = os.listdir(CACHE_LOCATION)
    except FileNotFoundError:
        os.mkdir(CACHE_LOCATION)
    if cleanname+".json" in existing_spells:
        # Users should be allowed to add custom spells, including spells
        # with personal names, to the cache.
        with open(CACHE_LOCATION+cleanname+".json", "r") as f:
            spell = json.load(f)
    elif cleanname_generic+".json" in existing_spells:
        with open(CACHE_LOCATION+cleanname_generic+".json", "r") as f:
            spell = json.load(f)
    else:
        r = requests.get(DND_API + "spells/" + cleanname_generic)
        if r.status_code == requests.codes.ok:
            spell = r.json()
        if spell:
            with open(CACHE_LOCATION+cleanname_generic+".json", "w") as f:
                json.dump(spell, f)
    if not spell:
        sys.stderr.write(f"Error: could not find the spell named \"{spellname}\".\n")
    return spell

def get_spell_list(stringlist):
    result = []
    for spellname in stringlist:
        spell = get_spell(spellname)
        if spell:
            result.append(spell)
        else:
            # get_spell already warns user that the spell wasn't found
            pass
    if len(stringlist) > len(result):
        sys.stderr.write(f"{len(stringlist)-len(result)} spell(s) not found.\n")
    return result

# Formatting functions
def spell_level_name(level):
    # Produce a string that indicates a spell level:
    # "Cantrip", "1st-level", ... , "9th-level"
    if level == 0:
        return "cantrip"
    else:
        return str(level) + {1:"st", 2:"nd", 3:"rd"}.get(level, "th") + "-level"
    
def format_summary(spell):
    # Produce the string e.g. "1st-level conjuration (concentration, ritual)"
    output = ""
    school = spell.get("school", {}).get("name", "[school]")
    if spell.get("level", -1) == 0:
        output = f"{school.title()} cantrip"
    else:
        output = f"{spell_level_name(spell.get('level', -1))} {school.lower()}"

    tags = []
    if spell.get("concentration", False):
        tags.append("concentration")
    if spell.get("ritual", False):
        tags.append("ritual")
    if tags:
        output += f" ({', '.join(tags)})"
    return output

def format_list(spelllist, single_spell_func, sort_by_level):
    # If sort_by_level is true, add a header e.g. "1st-Level Spells"
    # whenever the spell level changes.
    output = ""
    previous_spell_level = None
    for spell in spelllist:
        output += single_spell_func(spell, (sort_by_level and spell.get("level", -1) != previous_spell_level))
        previous_spell_level = spell.get("level", -1)
    return output

def format_md(spell, add_header):
    output = ""
    if add_header:
        if spell.get("level", -1) == 0:
            header = "Cantrips"
        else:
            header = spell_level_name(spell.get("level", -1)) + " Spells"
        output += f"{header}\n{'-'*len(header)}\n\n"
    output += f"""{spell.get("name", "???")}
{"-"*len(spell.get("name", "???"))}

*{format_summary(spell)}*

- **Casting Time:** {spell.get("casting_time", "N/A")}
- **Range:** {spell.get("range", "N/A")}
- **Components:** {", ".join(spell["components"]) if spell.get("components", None) else "None"}{(" (" + spell["material"] + ")") if spell.get("material", None) else ""}
- **Duration:** {spell.get("duration", "N/A")}

{(chr(10)+chr(10)).join(spell.get("desc", []))}
{(chr(10) + "*__At Higher Levels.__* " + (chr(10)+chr(10)).join(spell["higher_level"]) + chr(10)) if spell.get("higher_level", None) else ""}
***

"""
    return output

def format_tex(spell, add_header):
    # TODO escape occurences of '$' and '\'?
    output = ""
    if add_header:
        if spell.get("level", -1) == 0:
            header = "Cantrips"
        else:
            header = spell_level_name(spell.get("level", -1)) + " Spells"
        output += "\\section*{{{}}}\n".format(header)
    output += "\\subsection*{{{}}}\n\n".format(spell.get("name", "???"))
    output += "\\noindent\\textit{{{}}}\n\n".format(format_summary(spell))
    output += "\\begin{itemize}\n"
    output += "\\item \\textbf{{Casting Time:}} {}\n".format(spell.get("casting time", "N/A"))
    output += "\\item \\textbf{{Range:}} {}\n".format(spell.get("range", "N/A"))
    output += "\\item \\textbf{{Components:}} {}".format(", ".join(spell["components"]) if spell.get("components", None) else "None")
    output += (" ({})\n".format(spell["material"]) if spell.get("material", None) else "\n")
    output += "\\item \\textbf{{Duration:}} {}\n".format(spell.get("duration", "N/A"))
    output += "\\end{itemize}\n"
    output += "\\noindent "
    output += "\n\n".join(spell.get("desc",[]))
    output += "\n\n"
    if spell.get("higher_level", None):
        output += r"\noindent \textit{\textbf{At Higher Levels.}} "
        output += "".join(hl+"\n\n" for hl in spell.get("higher_level"))
    output += "\\hrule\n\n"
    return output

def format_html(spell, add_header):
    output = ""
    if add_header:
        if spell.get("level", -1) == 0:
            header = "Cantrips"
        else:
            header = spell_level_name(spell.get("level", -1)) + " Spells"
        output += f"""<h2 class="spell-level"><a name="level-{spell.get("level", -1)}">\
{html.escape(header)}</a></h2>\n"""
    output += f"""<h2 class="spell-name"><a name="{html.escape(clean_string(spell.get("_id", hash(spell.get("name", None)))))}">\
{html.escape(spell.get("name", "???"))}</a></h2>
<p><i>{html.escape(format_summary(spell))}</i></p>
<ul>
    <li><b>Casting Time:</b> {html.escape(spell.get("casting_time", "N/A"))}</li>
    <li><b>Range:</b> {html.escape(spell.get("range", "N/A"))}</li>
    <li><b>Components:</b> {html.escape(", ".join(spell["components"]) if spell.get("components", None) else "None")}
            {html.escape((" (" + spell["material"] + ")") if spell.get("material", None) else "")}</li>
    <li><b>Duration:</b> {html.escape(spell.get("duration", "N/A"))}</li>
</ul>

<p>{("</p>" + chr(10) + "<p>").join(html.escape(sd) for sd in spell.get("desc", []))}</p>
{(chr(10) + "<p><b><i>At Higher Levels.</i></b> " + ("</p>"+chr(10)+"<p>").join(html.escape(hl) for hl in spell["higher_level"]) + "</p>") if spell.get("higher_level", None) else ""}
<hr />
"""
    return output

def md_topntail(text):
    return """Spellbook
=========

""" + text.rstrip().rstrip("*") + "\n"

def tex_topntail(text):
    output = ""
    output += "\\documentclass{article}\n"
    output += "\\title{Spellbook}\n"
    output += "\\begin{document}\n"
    output += "\\maketitle\n"
    output += "%toc\n"
    text = text.rstrip()
    if text.endswith("\\hrule"):
        text = text[:-6]
    text += "\n\n"
    output += text
    output += r"\end{document}"
    return output

def html_topntail(text):
    output = ""
    output += r"""<!DOCTYPE html>
<html>
<head>
    <title>Spellbook</title>
    <style>
    body {
        font-family: Sans-Serif;
    }
    h2.spell-name {
    }
    h2.spell-name {
    }
    h2.toc-header {
        text-align:center;
        font-size:small;
    }
    li.toc-entry {
        font-size:small;
    }
    div.toc {
        display:table;
        padding: 1%;
        border: solid grey 1px;
    }
    </style>
</head>
<body>
<h1>Spellbook</h1>
<!--toc-->
"""
    text = text.strip()
    if text.endswith("<hr />"):
        text = text[:-7]
    output += text
    output += """
</body>
</html>"""
    return output

def md_toc(spelllist, text, sort_by_level):
    try:
        toc_index = text.index("=========\n") + 10
    except ValueError:
        sys.stderr.write("Failed to create table of contents.\n")
        return text
    first, last = text[:toc_index], text[toc_index:]
    toc_text = """
Contents
--------
"""
    previous_spell_level = None
    for spell in spelllist:
        spell_level = spell.get("level", -1)
        if spell_level != previous_spell_level and sort_by_level:
            if spell_level == 0:
                toc_text += f"> Cantrips:\n"
            else:
                toc_text += f"> {spell_level_name(spell_level)} Spells:\n"
        if sort_by_level:
            toc_text += ">"
        toc_text += f"> {spell.get('name', '???')}\n"
        previous_spell_level = spell_level
    return first + toc_text + last

def html_toc(spelllist, text, sort_by_level):
    try:
        toc_index = text.index("<!--toc-->\n")
    except ValueError:
        sys.stderr.write("Failed to create table of contents.\n")
        return text
    first, last = text[:toc_index], text[toc_index:]
    toc_text = "<div class='toc'>\n<h2 class='toc-header'>Contents</h2>\n<ul>\n"
    previous_spell_level = None
    for spell in spelllist:
        spell_level = spell.get("level", -1)
        if spell_level != previous_spell_level and sort_by_level:
            if previous_spell_level != None:
                toc_text += "</ul>\n"
            toc_text += "<li class='toc-entry'><a href='#level-"
            toc_text += html.escape(f"{spell.get('level', -1)}")
            toc_text += "'>"
            if spell_level == 0:
                toc_text += "Cantrips:"
            else:
                toc_text += f"{spell_level_name(spell_level)} Spells:"
            toc_text += "</a></li>\n<ul>"
        toc_text += "<li class='toc-entry'><a href='#"
        toc_text += f"{html.escape(clean_string(spell.get('_id', hash(spell.get('name', None)))))}"
        toc_text += f"'>{html.escape(spell.get('name', '???'))}</a></li>\n"
        previous_spell_level = spell_level
    if sort_by_level:
        toc_text += "</ul>"
    toc_text += "</ul></div>\n"
    return first + toc_text + last

def tex_toc(spelllist, text, sort_by_level):
    if "%toc\n" not in text:
        sys.stderr.write("Failed to create table of contents.\n")
        return text
    else:
        return text.replace("%toc\n", "\\tableofcontents\n")

# Main function
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_file",
        nargs="?",
        type=argparse.FileType("r"),
        default=sys.stdin,
        help="input file (default stdin)")
    parser.add_argument("-o",
            dest="output_filename",
            nargs=1,
            default=[""],
            help="output file (default stdout); should end with .html, .json, .md, or .tex")
    parser.add_argument("-s",
            dest="sort_by_level",
            action="store_const",
            const=True,
            default=False,
            help="sort spells by spell level, instead of alphabetically")
    parser.add_argument("-t",
            dest="toc",
            action="store_const",
            const=True,
            default=False,
            help="add a table of contents at the start of the file (.html, .md or .tex only)")
    args = parser.parse_args()
    post_process = []
    # post_process is a list of functions to sequentially call on a list of dicts to
    # change it into a string.
    if args.output_filename[0]:
        for suffix, functions in {
            ".md":  [
                (lambda x: format_list(x, format_md, args.sort_by_level)),
                md_topntail],
            ".html":[
                (lambda x: format_list(x, format_html, args.sort_by_level)),
                html_topntail],
            ".tex": [
                (lambda x: format_list(x, format_tex, args.sort_by_level)),
                tex_topntail],
        }.items():
            if args.output_filename[0].endswith(suffix):
                post_process.extend(functions)
        output_file = open(args.output_filename[0], "w")
    else:
        output_file = sys.stdout
    if len(post_process) == 0:
        post_process.append((lambda x: json.dumps(x, indent=4)))
    spelllist = get_spell_list(file_to_list(args.input_file))
    if args.sort_by_level:
        spelllist.sort(key=(lambda x: (x["level"], x["name"])))
    else:
        spelllist.sort(key=(lambda x: x["name"]))
    output = spelllist
    for function in post_process:
        output = function(output)
    if args.toc:
        for suffix, toc_function in {
            ".md": md_toc,
            ".html": html_toc,
            ".tex": tex_toc,
        }.items():
            if args.output_filename[0].endswith(suffix):
                output = toc_function(spelllist, output, args.sort_by_level)
                break
    output_file.write(output + "\n")
    return

if __name__=="__main__":
    main()
