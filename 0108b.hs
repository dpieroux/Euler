{-------------------------------------------------------------------------------

In the following equation x, y, and n are positive integers.
 
        1/x + 1/y = 1/n

For n = 4 there are exactly three distinct solutions:

        1/5 + 1/20 = 1/4
        1/6 + 1/12 = 1/4
        1/8 + 1/ 8 = 1/4

What is the least value of n for which the number of distinct solutions exceeds
one-thousand?

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

main = print $ head $ euler_108 1000