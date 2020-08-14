int add(int a, int b) {
    return a + b;
}

unsigned long long hailstone(unsigned long long n) {
    return (n % 2) ? ((3 * n) + 1)
                   : (n / 2);
}

void inplace_rot_13(char *s) {
    /* Mangles original string */
    while (*s != '\0') {
        if ((*s >= 'a' && *s <= 'm')
            || (*s >= 'A' && *s <= 'M'))
            *s += 13;
        else if ((*s >= 'n' && *s <= 'z')
            || (*s >= 'N' && *s <= 'Z'))
            *s -= 13;
        s++;
    }
    return;
}

int spin_wheels(int n) {
    /* Spend some time doing nothing */
    /* This takes 986 steps... */
    int steps = 0;
    unsigned long long num = 670617279;
    while (1) {
        steps++;
        num = hailstone(num);
        if (num == 1)
            break;
        if (num == 0)
            return 0; /* Ensure this doesn't get optimized away */
    }
    return steps;
}
