{-------------------------------------------------------------------------------

Investigating a Prime Pattern
Problem 146

The smallest positive integer n for which the numbers n²+1, n²+3, n²+7, n²+9,
n²+13, and n²+27 are consecutive primes is 10. The sum of all such integers n
below one-million is 1242490.

What is the sum of all such integers n below 150 million?

--------------------------------------------------------------------------------

It is easy to see that
  - n must be even, otherwise n²+1 is even and thus not prime.
  - because n is even, it must also be a multiple of 10; otherwise n²+1 or n²+9
    is a multiple of 5.
  - finally, n cannot be a multiple of 3 because then n²+3 would be a multiple
    of 3; and similarly for 7 and 13.
-------------------------------------------------------------------------------}

import Math.NumberTheory.Primes.Testing(isPrime)
import Data.List(foldl')

candidates :: Integer -> [Integer]
candidates bound = [10 * n | n <- [1 .. (div bound 10)-1]
                           , rem n 3 /= 0
                           , rem n 7 /= 0
                           , rem n 13 /= 0]

theChosen :: Integer -> [Integer]
theChosen bound = foldl' accum [] (candidates bound)
  where
    accum acc n = if all isPrime ns && not (any isPrime ns') then n:acc else acc
      where
        n²=n*n
        ns  = map (n²+) as
        ns' = map (n²+) as'

    as = [1, 3, 7, 9, 13, 27]
    as' = filter (\n -> not $ elem n as) [1, 3..27]


main = do
    putStrLn $ concat $ ["Euler 146,  1e6: ", show $ sum $ theChosen (10^6)]
    putStrLn $ concat $ ["Euler 146, 15e7: ", show $ sum $ theChosen (15*10^7)]