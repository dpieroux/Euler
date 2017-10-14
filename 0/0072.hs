import Data.List(foldl')
import Math.NumberTheory.Primes.Sieve(primes)
import Math.NumberTheory.Primes.Factorisation(factorise')

{-
Let d be a number whose prime factors are p1, p2, ... pN.

Because the p_i are primes, and thus relative primes with regards to each other,
the quantity N(d) of numbers that are not divisible by any p_i in the range 1..d
is given by
                 p1-1 p2-1     pN-1        d
        N(d) = d ---- ---- ... ---- = ----------- (p1-1)(p2-1)..(pN-1)
                  p1   p2       pN    p1 p2 .. pN

N(d) is thus also the number of reduced fractions having d as denominator.

Note that d/(p1 p2 .. pN) is an integer as the pX are its prime factors.
-}

nReducedFractions d = (d `div` (product ps)) * (foldl' (\r p -> r*(p-1)) 1 ps)
  where
    ps = map fst $ factorise' $ d

euler n = sum [nReducedFractions d | d <- [2..n]]

main = print $ euler 1000000

{-
In fact, this is just the sum of the Euler totients, which lead to the following
faster solution:

import Math.NumberTheory.Primes.Factorisation
main = print . sum $ map totient [2..1000000]
-}
