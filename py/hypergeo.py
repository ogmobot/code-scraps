#!/bin/python

def fact(n):
    if n <= 1:
        return 1
    else:
        return n * fact(n-1)

def choose(n, k):
    return fact(n)/(fact(k)*fact(n-k))

def hypergeo(k, N, K, n):
    # N = population size (cards in deck)
    # K = number of successes in population (good cards in deck)
    # n = number of draws (cards in hand)
    # k = number of observed successes (good cards in hand)
    return choose(K, k)*choose(N-K, n-k)/choose(N, n)

def main():
    N = int(input("Cards in deck: "))
    K = int(input("Good cards in deck: "))
    n = int(input("Cards in hand: "))
    k = int(input("Good cards you want in hand: "))
    print("Probability: {}%".format(round(100*hypergeo(k, N, K, n), 3)))
    return

if __name__=="__main__":
    main()
