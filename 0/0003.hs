import Math.NumberTheory.Primes.Factorisation (factorise)

main = print $ maximum $ map fst $ factorise 600851475143