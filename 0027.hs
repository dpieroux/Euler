{-
Euler discovered the remarkable quadratic formula:

    n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values 
n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible 
by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes
for the consecutive values n = 0 to 79. The product of the coefficients, −79 and
1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that
produces the maximum number of primes for consecutive values of n, starting with
n = 0.

Solution
--------

Let p_n = n² + an + b. To generate a long list of odd numbers, a and b must be 
odd. 

b = p_0 => b is a prime. 

As b must be odd, b is one of the primes from 3 up to 997 (the last prime before
1000). 

p_1 = 1 + a + b => a = p_1 - 1 - b 
a in [-999 999] => p_1 in [-998+b 1000+b] => p_1 is a prime <= 1000+b. 

Then, test all pairs (a, b) verifying what's written above and retain the one 
producing the longest sequences of prime numbers.
-}

import Data.List (foldl')
import Math.NumberTheory.Primes.Testing (isPrime)
import Math.NumberTheory.Primes.Sieve (primes)

{-------------------------------------------------------------------------------

best_a: 

Given a value for b, compute the value of 'a' for which p_n generates the 
longest prime sequence, within the allowed bounds.

input: b - the value of b

return: (a, l) - the value of a for which the length l of the sequence of primes
    generated by p_n is the longest

-------------------------------------------------------------------------------}
best_a :: Integer -> (Integer, Int)
best_a b = foldl' update (0, 0) p1s where 
    p1s = takeWhile (<(1000+b)) (tail primes)

    update :: (Integer, Int) -> Integer -> (Integer, Int)
    update (a, l) p1 = if l < l' then (a', l') else (a, l)
        where   
        a' = p1 - 1 - b
        l' = length (takeWhile isPrime (map (\n -> (n+a')*n + b) [2..]))


{-------------------------------------------------------------------------------

best_a_b

Compute the values of 'a' and 'b' for which p_n generates the longest sequence, 
within the allowed bounds.

return: (a, b, l) - the values of 'a' and 'b' for which the length 'l' of the 
    sequence of primes generated by p_n is the longest

-------------------------------------------------------------------------------}
best_a_b :: (Integer, Integer, Int)
best_a_b = foldl' update (0, 0, 0) (takeWhile (<997) (tail primes)) 
    where
    update (a, b, l) b' = if l < l' then (a', b', l') else (a, b, l) 
        where
        (a', l') = best_a b'

--------------------------------------------------------------------------------

euler_27 = a * b where (a, b, l) = best_a_b