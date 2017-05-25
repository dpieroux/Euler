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

To avoid similair solutions, we imposes x <= y, and thus 1/x >= 1/y .

Lemme 1:  n+1 <= x <= 2n <= y <= n(n+1)

            1/x + 1/y = 1/n  ==>  1/x <= 1/n  ==>  x > n  ==> n+1 <= x 
            1/x + 1/y = 1/n, y<=x  ==>  2/x >= 1/n  ==>  x <= 2n
            1/x + 1/y = 1/n, y<=x  ==>  2/y <= 1/n  ==>  y >= 2n
            1/x + 1/y = 1/n, n+1<=x ==> 1/(n+1) + 1/y >= 1/n  ==>  y <= n(n+1)

By posing x = n + d, with 1 <= d <= n, it comes 
    
            y = n + n²/d

For y to be integral, d must be a divisor of n². 

So, the principle of the algorithm is to count the divisor of n² smaller or
equal to n.

--------------------------------------------------------------------------------

See 0110b.hs for a much better approach.

-------------------------------------------------------------------------------}

import Data.List
import Math.NumberTheory.Primes.Factorisation

nbrSolutions :: Int -> Int
nbrSolutions n = length $ foldl' update [1] (primeFactors n')
  where
    n' = fromIntegral n :: Integer
    primeFactors :: Integer -> [(Integer, Int)]
    primeFactors n = factorise' (n*n)
    
    genFactors :: (Integer, Int) -> [Integer]
    genFactors (p, e) = [r | i <- [0..e], let r = p^i, r <= n']
    
    combine :: [Integer] -> [Integer] -> [Integer]
    combine as bs = [a*b | a <- as, b <- bs, let ab = a*b, ab <= n']

    update :: [Integer] -> (Integer, Int) -> [Integer]
    update acc pe = combine acc (genFactors pe)

euler_108 :: Int -> Int
euler_108 bound = head $ filter (\e -> bound < nbrSolutions e) [bound..]

main = print $ euler_108 1000