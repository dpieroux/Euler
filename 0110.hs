{-------------------------------------------------------------------------------

In the following equation x, y, and n are positive integers.
 
        1/x + 1/y = 1/n

It can be verified that when n = 1260 there are 113 distinct solutions and this
is the least value of n for which the total number of distinct solutions exceeds
one hundred.

What is the least value of n for which the number of distinct solutions exceeds
four million?

--------------------------------------------------------------------------------

Another solution is presented.

Let x = n + a and y = n + b.

1/x + 1/y = 1/n ==> n² = a b

Therefore, given n, there are as many solutions as pairs of divisor of n^2.

Given [(p_i, e_i)] the prime decomposition of n, the prime decomposition of n² is [(p_i, 2*e_i)] and the number of divisors nDiv = P_i (2*e_i+1).

The number of disctinct solutions is this (nDiv + 1) / 2, the +1 accounting for the case of square numbers 

-------------------------------------------------------------------------------}

import Data.List
import Math.NumberTheory.Primes.Factorisation

nbrSolutions :: Int -> Int
nbrSolutions n = div (foldl' update 1 (factorise' (fromIntegral n)) + 1) 2
  where  
    update :: Int -> (Integer, Int) -> Int
    update acc (p, e) = acc * (2*e+1)
    
euler_108 bound = filter (\e -> bound < nbrSolutions e) [bound..]

main = print $ head $ euler_108 4000000