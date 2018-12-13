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
\newcommand{\LOYALTY}{}
"""

CARDTEMPLATE["fitbox"] = r"""
\usepackage{environ}
\newdimen\fontdim
\newdimen\upperfontdim
\newdimen\lowerfontdim
\newif\ifmoreiterations
\fontdim12pt

\makeatletter
\NewEnviron{fitbox}[2]{% \begin{fitbox}{<width>}{<height>} stuff \end{fitbox}
  \def\buildbox{%
    \setbox0\vbox{\hbox{\minipage{#1}%
      \fontsize{\fontdim}{1.2\fontdim}%
      \selectfont%
      \stuff%
    \endminipage}}%
    \dimen@\ht0
    \advance\dimen@\dp0
  }
  \def\stuff{\BODY}% Store environment body
  \buildbox
  % Compute upper and lower bounds
  \ifdim\dimen@>#2
    \loop
      \fontdim.5\fontdim % Reduce font size by half
      \buildbox
    \ifdim\dimen@>#2 \repeat
    \lowerfontdim\fontdim
    \upperfontdim2\fontdim
    \fontdim1.5\fontdim
  \else
    \loop
      \fontdim2\fontdim % Double font size
      \buildbox
    \ifdim\dimen@<#2 \repeat
    \upperfontdim\fontdim
    \lowerfontdim.5\fontdim
    \fontdim.75\fontdim
  \fi
  % Now try to find the optimum size
  \loop
    %\message{Bounds: \the\lowerfontdim\space
    %         \the\fontdim\space \the\upperfontdim^^J}
    \buildbox
    \ifdim\dimen@>#2
      \moreiterationstrue
      \upperfontdim\fontdim
      \advance\fontdim\lowerfontdim
      \fontdim.5\fontdim
    \else
      \advance\dimen@-#2
      \ifdim\dimen@<10pt
        \lowerfontdim\fontdim
        \advance\fontdim\upperfontdim
        \fontdim.5\fontdim
        \dimen@\upperfontdim
        \advance\dimen@-\lowerfontdim
        \ifdim\dimen@<.2pt
          \moreiterationsfalse
        \else
          \moreiterationstrue
        \fi
      \else
        \moreiterationsfalse
      \fi
    \fi
  \ifmoreiterations \repeat
  \box0% Typeset content
}
% With thanks to Werner
"""

CARDTEMPLATE["start"] = r"""
\begin{tikzpicture}[x=1mm,y=1mm]
    \draw (0,0) rectangle (63,-88); % edge of card
    \draw (3,-3) rectangle (60,-82); % edge of border
    \draw (5,-3) node[below right] {\begin{fitbox}{48mm}{4mm}\parbox{48mm}{\CARDNAME \hfill \MANACOST}\end{fitbox}};
    \draw (6,-8) rectangle (57,-46); % box for artwork, 51mm x 38mm
    \draw (6.5,-8.5) node[inner sep=0pt,below right] {\includegraphics[width=50mm,height=37mm]{\ARTWORKFILE}};
    \draw (6,-46) node[below right] {\begin{fitbox}{48mm}{3mm}\TYPELINE\end{fitbox}};
    \draw[fill=white] (6,-51) rectangle (57,-85); % text box
""".strip()

CARDTEMPLATE["text"] = r"""
    \draw (6,-51) node[below right] {\begin{fitbox}{47mm}{32mm}{\parbox{47mm}{\CARDTEXT \par \textit{\FLAVOUR}}}\end{fitbox}};
""".strip()

CARDTEMPLATE["flavour"] = r"""
    \draw (6,-51) node[below right] {\begin{fitbox}{47mm}{32mm}{\parbox{47mm}{\textit{\FLAVOUR}}}\end{fitbox}};
""".strip()

CARDTEMPLATE["textflavour"] = r"""
    \draw (6,-51) node[below right] {\begin{fitbox}{47mm}{32mm}{\parbox{47mm}{\CARDTEXT \par \rule{textwidth}{.5pt} \par \textit{\FLAVOUR}}}\end{fitbox}};
"""

CARDTEMPLATE["powtou"] = r"""
    \draw (60,-85) node[above left,draw,fill=white] {\large \POWTOU};
""".strip("\n")

CARDTEMPLATE["loyalty"] = r"""
    \draw (60,-85) node[above left,draw,fill=white] {\large \hspace*{1ex}\LOYALTY\hspace*{1ex}};
""".strip("\n")

CARDTEMPLATE["finish"] = r"""
\end{tikzpicture}
""".strip()

def renewcommands(card):
    artwork_file = get_art(card)
    texstring = ""
    texstring += f"\\renewcommand{{\\CARDNAME}}{{{card.get('name','')}}}\n"
    texstring += f"\\renewcommand{{\\MANACOST}}{{{card.get('mana_cost','')}}}\n"
    texstring += f"\\renewcommand{{\\ARTWORKFILE}}{{{artwork_file}}}\n"
    texstring += f"\\renewcommand{{\\TYPELINE}}{{{card.get('type_line','')}}}\n"
    texstring += (f"\\renewcommand{{\\CARDTEXT}}{{{card.get('oracle_text','')}}}").replace("\n","{\\par}")+"\n"
    texstring += (f"\\renewcommand{{\\FLAVOUR}}{{{card.get('flavor_text','')}}}").replace("\n","{\\par}").replace("\"","``",1)+"\n"
    if "power" in card:
        texstring += f"\\renewcommand{{\\POWTOU}}{{{card['power']}/{card['toughness']}}}\n"
    if "loyalty" in card:
        texstring += f"\\renewcommand{{\\LOYALTY}}{{{card['loyalty']}}}\n"
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
    if len(card.get("oracle_text","").strip())>0:
        if "flavor_text" in card:
            output.append(CARDTEMPLATE["textflavour"])
        else:
            output.append(CARDTEMPLATE["text"])
    else:
        output.append(CARDTEMPLATE["flavour"])
    if "power" in card:
        output.append(CARDTEMPLATE["powtou"])
    if "loyalty" in card:
        output.append(CARDTEMPLATE["loyalty"])
    output.append(CARDTEMPLATE["finish"])
    return "\n".join(output)

def main():
    cardqueue = []
    for line in sys.stdin:
        card = get_card(line.strip())
        if card != None:
            if "card_faces" in card:
                cardqueue.extend(card["card_faces"])
            else:
                cardqueue.append(card)
    print(CARDTEMPLATE["fitbox"])
    print(CARDTEMPLATE["preamble"])
    for card in cardqueue:
        s = make_tex_card(card)
        print(s)
    return

if __name__=="__main__":
    main()
