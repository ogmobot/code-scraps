#!/usr/bin/python3

import sys
import os
from mtgfetch import get_card, CACHE_LOCATION, SCRYFALL_API, slugify

CARDTEMPLATE = {}
CARDTEMPLATE["preamble"] = r"""
\newcommand{\CARDNAME}{}
\newcommand{\MANACOST}{}
\newcommand{\ARTWORKFILE}{}
\newcommand{\TYPELINE}{}
\newcommand{\CARDTEXT}{}
\newcommand{\POWTOU}{}
\newcommand{\FLAVOUR}{}
"""

CARDTEMPLATE["start"] = r"""
\begin{tikzpicture}[x=1mm,y=1mm]
    \draw (0,0) rectangle (63,-88); % edge of card
    \draw (3,-3) rectangle (60,-82); % edge of border
    \draw (6,-3) node[below right] {\large \CARDNAME}
          (57,-3) node[below left] {\large \MANACOST};
    \draw (6,-8) rectangle (57,-46); % box for artwork, 51mm x 38mm
    \draw (6.5,-8.5) node[inner sep=0pt,below right] {\includegraphics[width=50mm,height=37mm]{\ARTWORKFILE}};
    \draw (6,-46) node[below right] {\TYPELINE};
    \draw[fill=white] (6,-51) rectangle (57,-85); % text box
    \draw (6,-51) node[below right] {\parbox{48mm}{\CARDTEXT \par \textit{\FLAVOUR}}};
""".strip() # TODO modify to allow planeswalker template

CARDTEMPLATE["powtou"] = r"""
\draw (60,-85) node[above left,draw,fill=white] {\large \POWTOU};
""".strip()

CARDTEMPLATE["finish"] = r"""
\end{tikzpicture}
""".strip()

def renewcommands(card):
    artwork_file = get_art(card)
    texstring = ""
    texstring += f"\\renewcommand{{\\CARDNAME}}{{{card['name']}}}\n"
    texstring += f"\\renewcommand{{\\MANACOST}}{{{card['mana_cost']}}}\n"
    texstring += f"\\renewcommand{{\\ARTWORKFILE}}{{{artwork_file}}}\n"
    texstring += f"\\renewcommand{{\\TYPELINE}}{{{card['type_line']}}}\n"
    texstring += (f"\\renewcommand{{\\CARDTEXT}}{{{card.get('oracle_text','')}}}").replace("\n","{\\par}")+"\n"
    texstring += (f"\\renewcommand{{\\FLAVOUR}}{{{card.get('flavor_text','')}}}").replace("\n","{\\par}").replace("\"","``",1)+"\n"
    if "power" in card:
        texstring += f"\\renewcommand{{\\POWTOU}}{{{card['power']}/{card['toughness']}}}\n"
    return texstring.strip()

def get_art(card):
    filename = slugify(card["name"])+".jpg"
    if filename not in os.listdir(CACHE_LOCATION):
        # download art
        if "card_faces" in card and "image_uris" in card["card_faces"][0]:
            url = card["card_faces"][0]["image_uris"]["art_crop"]
        elif "image_uris" in card:
            url = card["image_uris"]["art_crop"]
        else:
            url = "--help"
        os.chdir(CACHE_LOCATION)
        os.system("wget " + url + " -O " + filename)
    return filename

def make_tex_card(card):
    output = []
    output.append(renewcommands(card))
    output.append(CARDTEMPLATE["start"])
    if "power" in card:
        output.append(CARDTEMPLATE["powtou"])
    output.append(CARDTEMPLATE["finish"])
    return "\n".join(output)

def main():
    print(CARDTEMPLATE["preamble"])
    for line in sys.stdin:
        card = get_card(line.strip())
        s = make_tex_card(card)
        print(s)
    return

if __name__=="__main__":
    main()
