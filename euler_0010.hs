import Math.NumberTheory.Primes.Sieve(primes)

euler n = sum $ takeWhile (<n) primes

main = do
    print $ euler 10
    print $ euler 2000000