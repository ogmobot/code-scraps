/* Method of repeated squares */
unsigned long long modexp(unsigned long long b, unsigned long long e, unsigned long long m) {
	unsigned long long result = 1;
    while (e > 0) {
        if (e % 2) result = (result * b) % m;
        e >>= 1;
        b = (b * b) % m;
    }
    return result;
}
