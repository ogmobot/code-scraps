#include "raylib.h"
#include <stdbool.h>

#define BGCOLOUR WHITE
#define FGCOLOUR BLACK

#define BYTELENGTH 8

struct universe {
    unsigned int width;
    unsigned int height;
    bool wrap; /* If true, edge cells treat opposite edge as adjacent */
    unsigned char idx; /* current state in data[idx]; other used as buffer. */
    unsigned char *data[2];
    /* Each 8-bit char stores eight cells. Each row is (universe.width) bits long.
       There are (universe.height) rows in total. */
};

struct universe * create_universe(unsigned int width, unsigned int height, bool wrap) {
    struct universe *result = (struct universe *) RL_MALLOC(sizeof (struct universe));
    result->height = height;
    result->width = width;
    result->wrap = wrap;
    result->idx = 0;
    /* space needed: (width*height) / 8, round up. Do this by adding 7 before dividing, then rounding down. */
    for (int i = 0; i < 2; i++)
        (result->data)[i] = (unsigned char *) RL_MALLOC((sizeof (unsigned char)) * (((width * height) + (BYTELENGTH - 1)) / BYTELENGTH));
    return result;
}

void randomise_universe(struct universe *u) {
    unsigned int num_bytes = (((u->width * u->height) + (BYTELENGTH - 1)) / BYTELENGTH);
    /* Put random data into each buffer */
    /* On a UNIX-like system, it'd probably be faster to copy n bytes from /dev/random.
       That said, it's not like this functions needs to run fast. */
    for (int i = 0; i < 2; i++) {
        for (int n = 0; n < num_bytes; n++)
            u->data[i][n] = (unsigned char) GetRandomValue(0, (unsigned char) (-1));
            /* Assumes 2's complement representation, so -1 is max val */
    }
    return;
}

bool read_cell (struct universe *u, unsigned int x, unsigned int y) {
    /* This is probably slowing things down... */
    if (!(u->wrap) && ((x < 0) || (x >= u->width) || (y < 0) || (y >= u->height)))
        return 0;
    
    x %= u->width;
    y %= u->height;
    
    #define bit_index ((y * (u->width)) + x)
    return !!((((u->data)[u->idx])[bit_index / BYTELENGTH]) & (1 << (bit_index % BYTELENGTH)));
    #undef bit_index
}

void write_buffer (struct universe *u, unsigned int x, unsigned int y, bool value) {
    /* Doesn't change current state. Writes to data buffer u[!idx] instead. */
    /* Current code structure doesn't need to check if array is out of bounds */
    
    /*
    if ((x < 0) || (x >= u->width) || (y < 0) || (y >= u->height))
        return;
    */
    
    #define bit_index ((y * (u->width)) + x)
    if (value)
        (((u->data)[!u->idx])[bit_index / BYTELENGTH]) |= (1 << (bit_index % BYTELENGTH));
    else
        (((u->data)[!u->idx])[bit_index / BYTELENGTH]) &= ~(1 << (bit_index % BYTELENGTH));
    return;
    #undef bit_index
}

unsigned int count_neighbours(struct universe *u, unsigned int x, unsigned int y) {
    unsigned int result = 0;
    for (int i = -1; i <= 1; i++) {
        for (int j = -1; j <= 1; j++) {
            result += read_cell(u, x + i, y + j);
        }
    }
    result -= read_cell(u, x, y);
    return result;
};

void update_universe(struct universe *u) {
    /* Write updated state to the data buffer (e.g. data[1] if idx = 0) */
    unsigned int ncount = 0;
    for (unsigned int y = 0; y < u->height; y++) {
        for (unsigned int x = 0; x < u->width; x++) {
            ncount = count_neighbours(u, x, y);
            /* Hardcoded rules. TODO use a rule struct instead? */
            if (ncount == 3 || (read_cell(u, x, y) && (ncount == 2)))
                write_buffer(u, x, y, 1);
            else
                write_buffer(u, x, y, 0);
            /* Do this instead of write_buffer(u, x, y, (cond)) in case rule structs appear later */
        }
    }
    /* Current universe is now whatever is in the buffer */
    u->idx = !(u->idx);
    return;
}

void draw_universe(struct universe *u, unsigned int scalex, unsigned int scaley, int offsetx, int offsety, Color c) {
    /* Assumine BeginDrawing has already been called, and the screen wiped.
       Draw only living cells in universe. */
    for (unsigned int y = 0; y < u->height; y++) {
        for (unsigned int x = 0; x < u->width; x++) {
            if (read_cell(u, x, y))
                DrawRectangle(
                    (scalex * x) + offsetx, (scaley * y) + offsety,
                    scalex, scaley,
                    c
                );
        }
    }
    return;
}

int main() {
    // Initialization
    //--------------------------------------------------------------------------------------
    const int screenWidth = 600;
    const int screenHeight = 600;

    struct universe *u;
    u = create_universe(300, 300, true);
    randomise_universe(u);
    
    /*
    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 8; j++) {
            printf("%d ", read_cell(u, j, i));
        }
        printf("\n");
    }
    printf("\n");
    */

    InitWindow(screenWidth, screenHeight, "Conway's Game of Life");

    SetTargetFPS(30);               // Set our game to run at 30 frames-per-second
        
    // Initialization
    //--------------------------------------------------------------------------------------
    
    // Main game loop
    while (!WindowShouldClose()) {  // Detect window close button or ESC key
        // Update
        //----------------------------------------------------------------------------------
        update_universe(u);
        //----------------------------------------------------------------------------------

        // Draw
        //----------------------------------------------------------------------------------
        BeginDrawing();
        ClearBackground(BGCOLOUR);
        draw_universe(u, 2, 2, 0, 0, FGCOLOUR);
        DrawFPS(0, 0);
        EndDrawing();
                
        //----------------------------------------------------------------------------------
    }

    // De-Initialization
    //--------------------------------------------------------------------------------------
    CloseWindow();        // Close window and OpenGL context
    //--------------------------------------------------------------------------------------

    return 0;
}