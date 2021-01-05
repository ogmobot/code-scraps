/* Find expt(b, e) mod m, using method of repeated squares */
unsigned long modexp(unsigned long long b, unsigned long long e, unsigned long m) {
	unsigned long result = 1;
    while (e > 0) {
        if (e % 2) result = (result * b) % m;
        e /= 2;
        b = (b * b) % m;
    }
    return result;
}

/* Euclid's gcd algorithm */
unsigned long gcd(unsigned long a, unsigned long b) {
    unsigned long tmp;
    while (b != 0) {
        tmp = b;
        b = a % b;
        a = tmp;
    }
    return a;
}

int coprime(unsigned long a, unsigned long b) {
    return gcd(a, b) == 1LL;
}

/* "Borrowed" from Wikipedia */
unsigned long find_modular_inverse(unsigned long a, unsigned long n) {
    long long t = 0,
              new_t = 1,
              tmp_t,
              r     = n,
              new_r = a,
              tmp_r,
              quotient;
    while (new_r != 0) {
        quotient = r / new_r;

        tmp_t = new_t;
        new_t = t - (quotient * new_t);
        t = tmp_t;

        tmp_r = new_r;
        new_r = r - (quotient * new_r);
        r = tmp_r;
    }
    if (r > 1) return 0; /* Numbers aren't coprime */
    if (t < 0) t += n;
    return (unsigned long) t;
}
