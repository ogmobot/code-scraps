import time

def is_prime(p):
    # by trial division
    divisor = 2
    while divisor * divisor <= p:
        if p % divisor == 0: return False
        divisor += 1
    return True

def is_pseudo(p):
    return pow(2, p-1, p) == 1

def main():
    # Determine all primes beneath 2000 by trial division,
    # then determine the pseudoprimes beneath 2000.
    time_a = time.time()
    primes = [p for p in range(2, 200000) if is_prime(p)]
    time_b = time.time()
    pseudos = [p for p in range(2, 200000) if is_pseudo(p)]
    time_c = time.time()
    print("primes=>{} items in {} s".format(len(primes), time_b-time_a))
    print("pseudos=>{} items in {} s".format(len(pseudos), time_c-time_b))

if __name__=="__main__":
    main()
