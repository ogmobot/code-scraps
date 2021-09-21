import ctypes
import os

Puzzle_Grid = ctypes.c_byte * 81
working_directory = os.path.dirname(os.path.realpath(__file__))
c_lib = ctypes.CDLL(f"{working_directory}/sudoku.so")

def print_sudoku(grid):
    for row in range(9):
        print(" ".join(
            (
                lambda v: str(v) if v != 0 else "_"
            )(
                grid[(9*row) + col]
            ) for col in range(9)))

def load_puzzles(filename):
    # Puzzle file should have the format:
    #   Title 1
    #   003020600
    #   900305001
    #   (6 more lines)
    #   005010300
    #   Title 2
    #   ...
    #   etc.
    # Read stops at EOF or first blank line
    puzzles = []
    with open(filename, "r") as puzzlefile:
        while True:
            title = puzzlefile.readline().strip()
            if not title: break
            tup = (title, [])
            for _ in range(9):
                tup[1].extend(
                    [int(c) for c in puzzlefile.readline().strip()]
                )
            puzzles.append(tup)
    return [(name, Puzzle_Grid(*p)) for name, p in puzzles]

def main():
    # function expects uint8_t*
    puzzles = load_puzzles("puzzles.txt")
    solution = Puzzle_Grid()
    for i, tup in enumerate(puzzles):
        c_lib.solve(tup[1], solution)
        print(tup[0])
        print_sudoku(solution)

if __name__ == "__main__":
    main()
