{-
================================================================================
The proper divisors of a number are all the divisors excluding the number
itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the
sum of these divisors is equal to 28, we call it a perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum of the
proper divisors of 284 is 220, forming a chain of two numbers. For this reason,
220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with 12496, we
form a chain of five numbers:

12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)

Since this chain returns to its starting point, it is called an amicable chain.

Find the smallest member of the longest amicable chain with no element
exceeding one million.
================================================================================
-}

import Data.List
import Data.Numbers.Primes

type Base = Int
type Exp = Int
type Factor = (Base, Exp)
type Prime = Int

-- Returns the prime factorisation of a number
primeFactors :: Int -> [Factor] 
primeFactors n = primeFactors' n (map fromIntegral primes) []
 
primeFactors' :: Int -> [Prime] -> [Factor] -> [Factor]
primeFactors' n (p:ps) acc
    | n == 1       = acc
    | n < p*p      = (n, 1):acc
    | rem n p == 0 = primeFactors' n' ps ((p, exp):acc)
    | otherwise    = primeFactors' n ps acc
  where
    (n', exp) = reduce (div n p) p 1

reduce n p exp
    | r == 0 = reduce q p (exp+1)
    | otherwise = (n, exp)
  where
    (q, r) = quotRem n p

-- Returns the divisors of a number
divisors :: Int -> [Int]
divisors n = combine $ primeFactors n

expand :: Factor -> [Int]
expand (base, exp) = expand' base exp base [1]

expand' base 0 cur acc = acc 
expand' base exp cur acc = expand' base (exp-1) (cur*base) (cur:acc) 

combine :: [Factor] -> [Int]
combine factors = combine' factors [1]

combine' :: [Factor] -> [Int] -> [Int]
combine' [] acc = acc
combine' (factor:factors) acc = combine' factors (combine'' (expand factor) acc)

combine'' :: [Int] -> [Int] -> [Int]
combine'' factors values = concat $ map (\f -> map (f*) values) factors
     



