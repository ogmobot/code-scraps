#include <stdio.h>
#include <stdbool.h>

#define MAX_ELEMENTS 1024

enum elements {
    /* Base Element */
    E_EARTH,
    E_WATER,
    E_AIR,
    E_FIRE,
    E_QUINT,

    /* Classical Property */
    E_COLD,
    E_HOT,
    E_WET,
    E_DRY,

    /* L1 Compound */
    E_MUD,
    E_DUST,
    E_ICE,
    E_ALCOHL,
    E_LIGHT,
    E_PLASMA,
    E_BREATH,
    E_SPARKS,

    /* L2 Compound */
    E_CLAY,
    E_SAND,
    E_SNOW,
    E_LAMP,
    E_GLASS,
    E_LIGHTN,
    E_BUBBLE,

    /* Life */
    E_BODY,
    E_LIFE,

    /* Array Terminator */
    E_INVALID
};

enum elements reactions[][3] = {
    /* Base Element + Base Element = Classical Property */
    {E_EARTH,   E_WATER,    E_COLD},
    {E_EARTH,   E_FIRE,     E_DRY},
    {E_WATER,   E_AIR,      E_WET},
    {E_AIR,     E_FIRE,     E_HOT},

    /* Base Element + Classical Property = L1 Compound */
    {E_EARTH,   E_COLD,     E_MUD},
    {E_EARTH,   E_DRY,      E_DUST},
    {E_WATER,   E_COLD,     E_ICE},
    {E_WATER,   E_WET,      E_ALCOHL},
    {E_FIRE,    E_DRY,      E_LIGHT},
    {E_FIRE,    E_HOT,      E_PLASMA},
    {E_AIR,     E_WET,      E_BREATH},
    {E_AIR,     E_HOT,      E_SPARKS},

    /* L1 Compound + Simpler Element = L2 Compound */
    {E_MUD,     E_HOT,      E_CLAY},
    {E_DUST,    E_HOT,      E_SAND},
    {E_ICE,     E_AIR,      E_SNOW},
    {E_ALCOHL,  E_FIRE,     E_LAMP},
    {E_LIGHT,   E_DRY,      E_GLASS},
    {E_PLASMA,  E_SPARKS,   E_LIGHTN},
    {E_BREATH,  E_WATER,    E_BUBBLE},

    /* Life */
    /* For you were made from dust, and to dust you will return.
     * Gen 3:19 */
    /* (earth+fire) + (air+water) */
    {E_DUST,    E_BREATH,   E_BODY},
    {E_QUINT,   E_BODY,     E_LIFE},

    /* Array terminator */
    {E_INVALID, E_INVALID, E_INVALID}
};

bool element_known[MAX_ELEMENTS];

void setup_known() {
    element_known[E_EARTH] = true;
    element_known[E_WATER] = true;
    element_known[E_AIR] = true;
    element_known[E_FIRE] = true;
    return;
}

char* element_name(enum elements e) {
    switch (e) {
        case E_EARTH:
            return "earth";
        case E_WATER:
            return "water";
        case E_AIR:
            return "air";
        case E_FIRE:
            return "fire";
        case E_QUINT:
            return "quintessence";
        case E_COLD:
            return "cold";
        case E_HOT:
            return "hot";
        case E_WET:
            return "wet";
        case E_DRY:
            return "dry";
        case E_MUD:
            return "mud";
        case E_DUST:
            return "dust";
        case E_ICE:
            return "ice";
        case E_ALCOHL:
            return "alcohol";
        case E_LIGHT:
            return "light";
        case E_PLASMA:
            return "plasma";
        case E_BREATH:
            return "breath";
        case E_SPARKS:
            return "sparks";
        case E_CLAY:
            return "clay";
        case E_SAND:
            return "sand";
        case E_SNOW:
            return "snow";
        case E_LAMP:
            return "lamp";
        case E_GLASS:
            return "glass";
        case E_LIGHTN:
            return "lightning";
        case E_BUBBLE:
            return "bubble";
        default:
            return "!unknown element!";
    }
}

void display_known() {
    printf("==Known elements==\n");
    for (int i = 0; i < MAX_ELEMENTS; i++) {
        if (element_known[i]) {
            printf("  %2d = %s\n", i, element_name(i));
        }
    }
    printf("\n");
    return;
}

void alchemise() {
    int first, second;
    printf("Select two elements.\n> ");
    scanf("%d%d", &first, &second);
    if (!element_known[first]) {
        printf("You don't know that element (%d).\n", first);
        return;
    }
    if (!element_known[second]) {
        printf("You don't know that element (%d).\n", second);
        return;
    }
    for (int i = 0; reactions[i][0] != E_INVALID; i++) {
        if ((reactions[i][0] == first) && (reactions[i][1] == second)
         || (reactions[i][1] == first) && (reactions[i][0] == second)){
            /* Success! */
            element_known[reactions[i][2]] = true;
            printf("The reaction was successful!\n");
            printf("%s (%d) + %s (%d) => %s (%d)\n",
                    element_name(reactions[i][0]), (int) reactions[i][0],
                    element_name(reactions[i][1]), (int) reactions[i][1], 
                    element_name(reactions[i][2]), (int) reactions[i][2]);
            return;
        }
    }
    /* This doesn't match any of the reactions. */
    printf("Those two elements don't mix.\n");
    return;
}

int main() {
    bool running = true;
    int response;
    setup_known();
    while (running) {
        printf(" 1 to check elements.\n"
               " 2 to attempt a reaction.\n"
               "-1 to quit.\n"
               "> ");
        scanf("%d", &response);
        switch (response) {
            case 1:
                display_known();
                break;
            case 2:
                alchemise();
                break;
            case -1:
                running = false;
                break;
            default:
                break;
        }
    }
    return 0;
}
