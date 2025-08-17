# cw structure
# {
#   "width": 15,
#   "height":15,
#   (0, 0): 'A',
#   (0, 1): 'B',
#   "starts": {(0, 0)}
# }

# "Black square" character
XXX = "*"

# "Needs letter" character
YYY = "?"

_EXAMPLE_CW = {
    "width": 5,
    "height":3,
    (0, 0): XXX, (0, 1): XXX, (0, 2): "E", (0, 3): "X", #(0, 4): XXX,
    (1, 0): "E", (1, 1): "S", (1, 2): "T", (1, 3): XXX, (1, 4): "T",
    (2, 0): XXX, (2, 1): "P", (2, 2): "A", (2, 3): "R", (2, 4): "T"
}

EXAMPLE_CW = {
    "width": 9,
    "height":9
}

CWCOLOURS = {
    # default white
    XXX: "black", # black square
    YYY: "beige", # square that needs text
}
CWFONT = ("Times", 18)
CWSQUARE_W = 2
CWSQUARE_H = 1

DIR_H = 0
DIR_V = 1

TEX_COLSIZE = 8

import tkinter as tk

def e_addword(event):
    pass

# Unused
def addword(cw, row, col, direction, word):
    if direction == DIR_H:
        delta = (0, 1)
    elif direction == DIR_V:
        delta = (1, 0)
    else:
        raise ValueError("invalid direction arg")
    loc = (row, col)
    for letter in word:
        cw[loc] = letter
        loc = (loc[0] + delta[0], loc[1] + delta[1])
    # Automatically add a black square at the end of the word,
    # to indicate the word ends there
    if loc[0] < cw.get("height", 0) and loc[1] < cw.get("width", 0):
        cw[loc] = XXX
    return True # TODO: return false on collision or out of bounds

def e_keypress(event):
    if event.char.isalnum():
        event.widget.configure(text=event.char.upper())
        # Mark rotationally symmetric cell with YYY if it's empty
        mark_symmetric(event.widget)
    elif event.keycode >= 37 and event.keycode <= 40:
        #directional keys:
        # left  37
        # up    38
        # right 39
        # down  40
        dc = {37: -1, 39: 1}.get(event.keycode, 0)
        dr = {38: -1, 40: 1}.get(event.keycode, 0)
        # change focus to corresponding cell
        grid_dict = event.widget.grid_info()
        r = int(grid_dict.get("row", -1))
        c = int(grid_dict.get("column", -1))
        if (r+dr, c+dc) in event.widget.parent_dict:
            event.widget.parent_dict[(r+dr, c+dc)].focus_set()
    else:
        event.widget.configure(text=XXX)
        mark_symmetric(event.widget)
        #print("Key pressed has code {}".format(event.keycode))
    pass

def mark_symmetric(widget):
    grid_dict = widget.grid_info()
    entries = widget.parent_dict
    cw_tmp = grid_to_cw(entries)
    r = int(grid_dict.get("row", -1))
    c = int(grid_dict.get("column", -1))
    width = cw_tmp["width"]
    height = cw_tmp["height"]
    if (height-r-1, width-c-1) not in entries:
        # Should never happen
        raise KeyError("missing cell at row={}, col={}?".format(height-r-1, width-c-1))
    if widget.cget("text") != XXX:
        if entries[(height-r-1, width-c-1)].cget("text") == XXX:
            entries[(height-r-1, width-c-1)].configure(
                text=YYY,
                background=CWCOLOURS.get(YYY, "white"))
    else:
        if entries[(height-r-1, width-c-1)].cget("text") == YYY:
            entries[(height-r-1, width-c-1)].configure(
                text=XXX,
                background=CWCOLOURS.get(XXX, "white"))
    return

def newgrid(master, cw):
    entries = {}
    for r in range(cw.get("height", 0)):
        for c in range(cw.get("width", 0)):
            e = tk.Label(
                master,
                text=cw.get((r, c), XXX),
                font=CWFONT,
                width=CWSQUARE_W,
                height=CWSQUARE_H,
                takefocus=True,
                borderwidth=1,
                activebackground="yellow")
            e.bind("<Button-1>", lambda ev: ev.widget.focus_set())
            e.bind("<Key>", e_keypress)
            e.bind("<FocusIn>", lambda ev: ev.widget.configure(state=tk.ACTIVE))
            e.bind("<FocusOut>", lambda ev: ev.widget.configure(
                state=tk.NORMAL,
                background=CWCOLOURS.get(ev.widget.cget("text"), "white")
            ))
            e.configure(background="black" if e.cget("text") == XXX else "white")
            e.grid(row=r, column=c)
            # parent_list is a new identifier
            e.parent_dict = entries
            entries[(r, c)] = e
    return entries

def grid_to_cw(entries):
    # turns Tk label grid into crossword
    cw = {}
    for k, v in entries.items():
        cw[k] = v.cget("text")
    entry_keys = entries.keys()
    cw["height"] = max(k[0] for k in entry_keys) - min(k[0] for k in entry_keys) + 1
    cw["width"] = max(k[1] for k in entry_keys) - min(k[1] for k in entry_keys) + 1
    return cw

def find_words(cw):
    # turns crossword = {(0,0):'a', (0,1):'b', (1,0):'c', (1,1):'d'}
    # into {(0,0,DIR_H):'ab', (0,0,DIR_V):'ac', ...}
    result = {}
    # search for horizontal words
    for row in range(cw["height"]):
        for col in range(cw["width"]):
            if cw.get((row,col), XXX) != XXX and cw.get((row,col-1), XXX) == XXX and cw.get((row,col+1), XXX) != XXX:
                # this is the start of a word
                word = ""
                r, c = row, col
                while cw.get((r,c), XXX) != XXX:
                    word += cw[(r, c)]
                    c += 1
                result[(row, col, DIR_H)] = word
    # search for vertical words
    for row in range(cw["height"]):
        for col in range(cw["width"]):
            if cw.get((row,col), XXX) != XXX and cw.get((row-1,col), XXX) == XXX and cw.get((row+1,col), XXX) != XXX:
                # this is the start of a word
                word = ""
                r, c = row, col
                while cw.get((r,c), XXX) != XXX:
                    word += cw[(r, c)]
                    r += 1
                result[(row, col, DIR_V)] = word
    return result

def export_tex(cw):
    words = find_words(cw)
    marked_cells = dict(set((t[:2],None) for t in words.keys()))
    header = "\\begin{{Puzzle}}{{{}}}{{{}}}%\n".format(cw["width"], cw["height"])
    footer = "\\end{Puzzle}"

    counter = 1

    body = ""
    for row in range(cw["height"]):
        rowtext = ""
        for col in range(cw["width"]):
            coltext = "|"
            if (row,col) in marked_cells:
                coltext += "[{}]".format(counter)
                if not marked_cells.get((row,col),True):
                    marked_cells[(row,col)] = counter
                counter += 1
            coltext += cw.get((row,col), XXX)
            coltext += " "*(TEX_COLSIZE - len(coltext))
            rowtext += coltext
        rowtext += "|.\n"
        body += rowtext

    clues_across = {}
    clues_down = {}
    for loc, word in words.items():
        if loc[2] == DIR_H:
            clues_across[marked_cells.get(loc[:2],None)] = "\\Clue{{{}}}{{{}}}{{{} ({})}}\\\\".format(marked_cells.get(loc[:2],None), word, word[::-1].title(), len(word))
        elif loc[2] == DIR_V:
            clues_down[marked_cells.get(loc[:2],None)] = "\\Clue{{{}}}{{{}}}{{{} ({})}}\\\\".format(marked_cells.get(loc[:2],None), word, word[::-1].title(), len(word))
        else:
            raise ValueError("Weird direction for word \"{}\" ({})".format(word, loc))
    clues = ""
    clues += "\n\n\\begin{PuzzleClues}{\\textbf{Across}}\\\\%\n"
    clues += "\n".join(clues_across[k] for k in sorted(clues_across.keys()))
    clues += "\n\\end{PuzzleClues}%\n\n\\begin{PuzzleClues}{\\textbf{Down}}\\\\%\n"
    clues += "\n".join(clues_down[k] for k in sorted(clues_down.keys()))
    clues += "\n\\end{PuzzleClues}%"
    
    return (header + body + footer + clues)

def reset_grid(master):
    #result = ask_question("New width: ", "New height: ")
    result = (input("New width: "), input("New height: "))
    if result == None:
        return
    w = int(result[0])
    h = int(result[1])
    for child in master.winfo_children():
        child.destroy()
    return newgrid(master, {"height": h, "width": w})

# Unused
def ask_question(*prompts):
    # spawns a new window
    #   prompt0  [ textbox0 ]
    #   prompt1  [ textbox1 ] ...
    #   [ OK ]
    result = []
    newWindow = tk.Tk()
    promptframe = tk.Frame(newWindow)

    answers = []
    for i, p in enumerate(prompts):
        tk.Label(promptframe, text=p).grid(row=i, column=0)
        e = tk.Entry(promptframe)
        e.grid(row=i, column=1)
        answers.append(e)
    
    bok = tk.Button(
        newWindow,
        text="OK",
        command=lambda: (result.extend(answer.get() for answer in answers)))
    promptframe.pack()
    bok.pack()
    newWindow.mainloop()
    return result

def func_wrapper(f, args, resultdict):
    # Applies f to args and stores the result in the given dictionary.
    # f must return a dictionary.
    # Original contents of dictionary are destroyed.
    resultdict.clear()
    resultdict.update(f(*args))
    return

def main():
    root = tk.Tk()
    mainframe = tk.Frame(root)
    entries = newgrid(mainframe, EXAMPLE_CW)
    b1 = tk.Button(
        root,
        text="New crossword",
        command=lambda: func_wrapper(reset_grid, [mainframe], entries))
    b2 = tk.Button(
        root,
        text="Export as .tex",
        command=lambda: print(export_tex(grid_to_cw(entries))))
    mainframe.pack()
    b1.pack()
    b2.pack()
    root.mainloop()
    #root.destroy()

if __name__ == "__main__":
    main()
