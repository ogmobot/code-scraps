#include <stdio.h>
#include <inttypes.h> /* for fixed-width integer types */
#include <stdlib.h> /* for generating (insecure) pseudorandom numbers */
#include <time.h> /* for generating (insecure) pseudorandom numbers */

extern unsigned long modexp(unsigned long b, unsigned long e, unsigned long m);
extern int coprime(unsigned long a, unsigned long b);
extern unsigned long find_modular_inverse(unsigned long a, unsigned long n);

struct crypto_keys {
    unsigned long modulo; /* large semiprime pq */
    unsigned long encryption_key; /* psuedorandom number coprime to (p-1)(q-1) */
    unsigned long decryption_key; /* inverse to encryption key mod (p-1)(q-1) */
};

struct key {
    unsigned long modulo;
    unsigned long value;
};

/* "Secret" prime numbers */
/*
const unsigned long p = 5479;
const unsigned long q = 6343;
*/
const unsigned long p = 92333;
const unsigned long q = 27127;

struct crypto_keys generate_new_keys() {
    struct crypto_keys result;
    const unsigned long m = (p - 1)*(q - 1);
    result.modulo = p * q;
    unsigned long encryption_key = ((unsigned long) random()) % m;
    while (!coprime(encryption_key, m)) {
        encryption_key++;
    }
    result.encryption_key = encryption_key;
    result.decryption_key = find_modular_inverse(encryption_key, m);
    return result;
}

uint32_t encode_u32(uint32_t n, struct key k) {
    /* Encode something that looks like an (4-byte) unsigned long */
    /* This fails catastrophically if n > k.modulo */
    return (uint32_t) modexp(n, k.value, k.modulo);
}

void encode_msg_to_buffer(char * msg, char * buffer, struct key k) {
    /* Assume char is 8 bits */
    /* Length of buffer must be at least as big as the string in msg,
       rounded up to the nearest multiple of 4, plus 4.
       So, a 101-char message would require 104+4 = 108 bytes.
       Ideally, both strings have a lot of spare room; in this example,
       something like 256 bytes each would be fine. */
#define CHUNK_SIZE 4
    unsigned char chunk[CHUNK_SIZE] = {};
    uint32_t * ptr = (uint32_t *) &chunk;
    uint32_t * buf = (uint32_t *) buffer;
    for (int i = 0; msg[i] != '\0'; i++) {
        chunk[i%CHUNK_SIZE] = msg[i];
        if (i % CHUNK_SIZE == CHUNK_SIZE - 1) {
            /* printf("Encrypting %u => %u\n", *ptr, encode_u32(*ptr, k)); */
            *buf = encode_u32(*ptr, k);
            buf++;
            *ptr = 0;
        }
    }
    if (*ptr) {
        /* Not everything got encoded */
        *buf = encode_u32(*ptr, k);
        buf++;
    }
    *buf = 0;
    return;
#undef CHUNK_SIZE
}

void print_as_hex(char * buffer) {
    for (int i = 0; buffer[i] != '\0'; i++) {
        printf("%2c", buffer[i]);
    }
    printf("\n");
    for (int i = 0; buffer[i] != '\0'; i++) {
        printf("%02x", buffer[i]%0x100);
    }
    printf("\n");
    return;
}

int main() {
    srandom(time((void *) 0));
    /*
	printf("4^132 mod 497 = %lu (= 379)\n", modexp(4, 132, 497));
    */

    struct crypto_keys test_keys = generate_new_keys();
    printf("Test key:\n"
           ".modulo=%lu, .encryption_key=%lu, .decryption_key=%lu\n",
        test_keys.modulo, test_keys.encryption_key, test_keys.decryption_key);

    /*
    unsigned long num = modexp(497, test_keys.encryption_key, test_keys.modulo);
    printf("Encrypting 497 => %lu\n", num);
    printf("Decrypting %lu => %lu\n", num, modexp(num, test_keys.decryption_key, test_keys.modulo));
    */

    char buffer[256];
    char msg[256];
    struct key encryptor;
    encryptor.modulo = test_keys.modulo;
    encryptor.value = test_keys.encryption_key;
    struct key decryptor;
    decryptor.modulo = test_keys.modulo;
    decryptor.value = test_keys.decryption_key;

    sprintf(msg, "Hello, world!");
    printf("Original message:\n");
    print_as_hex(msg);
    encode_msg_to_buffer(msg, buffer, encryptor);
    printf("Encrypted message:\n");
    print_as_hex(buffer);
    encode_msg_to_buffer(buffer, msg, decryptor);
    printf("Decrypted message:\n");
    print_as_hex(msg);
	return 0;
}
