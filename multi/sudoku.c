#include <stdbool.h>
#include <stdint.h>

#define celref(row, col) (9*(row)) + (col)
#define row_of(index) ((index) / 9)
#define col_of(index) ((index) % 9)

/* A sudoku puzzle is a uint8_t array of length 81.
 * Use 0 to represent blank cells.
 */

void copy_array(uint8_t * from_array, uint8_t * to_array) {
    /* Copies values from one array to the other */
    for (uint8_t i = 0; i < 81; i++)
        to_array[i] = from_array[i];
}

bool allowed(uint8_t * puzzle, uint8_t index, uint8_t val) {
    /* Returns whether a value may be placed at a certain index,
     * given the existing values in the puzzle.
     * Assumes the given location currently contains 0.
     */

    /* Check row contains no instances of this value */
    for (
        uint8_t i = celref(row_of(index), 0);
        i < celref(row_of(index) + 1, 0);
        i++
    )
        if (puzzle[i] == val) return false;
    /* Check row contains no instances of this value */
    for (uint8_t i = celref(0, col_of(index)); i < 81; i += 9)
        if (puzzle[i] == val) return false;
    /* Check square contains no instances of this value */
    for (uint8_t subrow = 0; subrow < 3; subrow++)
        for (uint8_t subcol = 0; subcol < 3; subcol++)
            if (puzzle[celref(
                ((row_of(index) / 3) * 3) + subrow,
                ((col_of(index) / 3) * 3) + subcol
            )] == val) return false;
    return true;
}

void solve(uint8_t * puzzle, uint8_t * solution) {
    /* Solve puzzle and write solution to buffer.
     * `puzzle` and `solution` should be uint8_t arrays of length 81.
     * Mangles the `puzzle` buffer.
     */
    for (uint8_t i = 0; i < 81; i++)
        if (puzzle[i] == 0) {
            for (uint8_t val = 1; val <= 9; val++)
                if (allowed(puzzle, i, val)) {
                    puzzle[i] = val;
                    solve(puzzle, solution);
                    puzzle[i] = 0;
                }
            return;
        }
    /* No blank cells found -- this is a solution */
    copy_array(puzzle, solution);
    return;
}

