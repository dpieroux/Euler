{-------------------------------------------------------------------------------

In the following equation x, y, and n are positive integers.
 
        1/x + 1/y = 1/n

It can be verified that when n = 1260 there are 113 distinct solutions and this
is the least value of n for which the total number of distinct solutions exceeds
one hundred.

What is the least value of n for which the number of distinct solutions exceeds
four million?

--------------------------------------------------------------------------------

See Euler 108b.

Let x = n + a and y = n + b. Then 1/x + 1/y = 1/n ==> n² = a b

Therefore, given n, there are as many solutions as pairs of divisor of n^2.

Given [(p_i, e_i)] the prime decomposition of n, the prime decomposition of n²
is [(p_i, 2*e_i)] and its number of divisors nDiv = Product(2*e_i+1, i).

nDiv is always odd because (n, n) is a degenerated pair of divisors of n².
Therefore, the number of distinct pairs of divisors, (a,b) being equivalent to
(b,a), is (nDiv + 1) / 2.

This is also the number of distinct solutions to the equation 1/x + 1/y = 1/n.

In addition, given a finite set of values to be used as exponents in a prime
decomposition,
  - the number of divisors of any number obtained by combining these exponents
    with primes is independent of which prime is associated with which exponent;
  - the smallest number n is obtained by associating the largest v's to the
    smallest primes.

In the problem 108b, the idea was to consider increasing values of n until the
first one displaying more solutions than the one required. This required
computing the prime decomposition of all these numbers.

Here we work directly with the prime decomposition of the candidate n.

In practice, we consider finite sequence of numbers (e0, e1, e2, ..., en)
representing the power of the primes. Since we are looking for the smallest
number having more than the requested number of divisors, we are also
restraining ourselves to non-increasing sequences of exponents:
*       i < j ==> e_i >= e_j

--------------------------------------------------------------------------------

Note: the approach here allows to find the solution quickly, but not to check that there isn't a better one... See 0110b.hs for that.

-------------------------------------------------------------------------------}

import Data.Maybe
import Data.List
import Math.NumberTheory.Primes.Sieve

import Debug.Trace

-- Now we need a way to iterate through these finite sequences.
--
-- For that we will consider sequence of increasing sum S({ei}) := Sum (e_i, i).
--
-- Given a sum S, the first exponent e0 can vary from 1 to S, the second from 1
-- to e1, the third from 1 to e2, until the sum is obtained. This invites for a
-- recursive definition.


genExpoSeq :: Int -> [[Int]]
genExpoSeq 0 = [[]]
genExpoSeq sum = concat [ map (e:) ess | e <- [1 .. sum]
                        , let ess = genExpoSeq (sum-e) 
                        ]

-- It is necessary to filter out the sequences leading to numbers n that have
-- not more distinct solutions than the given bound the required bound b:

nbrSolutions :: [Int] -> Int
nbrSolutions = (`div` 2) . (+1) . product . map (\e -> 2*e+1)

rejectBelow :: Int -> [[Int]] -> [[Int]]
rejectBelow bound = filter (\es -> bound < nbrSolutions es)

-- Given the exponent sum S, the largest number of distinct solutions is
-- obtained by the sequence e0 = ... = eS = 1, which displays (3^S + 1)/2
-- solutions. This gives a lower bound for S below which there is no acceptable
-- solution:
-- *        bound < (3^S + 1)/2  ==>  2*bound <= 3^S

lowerExponentsSum :: Int -> Int
lowerExponentsSum bound = fst . head 
             $ filter ((2*bound <=) . snd) 
             $ zip [0..] (iterate (*3) 1)

upperBoundForN :: Int -> Integer
upperBoundForN expoSum = exponentsToNumber $ take expoSum $ repeat 1

-- Given the sum S, the smallest number that can be generated is 2^S. Therefore,
-- as soon as a candidate n* is found, there is no need to search for better
-- solutions if 2^S >= n*.


iter :: Int -> Integer
iter bound = iter' bound (lowerExponentsSum bound) candidate
  where
    exponentsSum = lowerExponentsSum bound
    candidate    = upperBoundForN exponentsSum

iter' :: Int -> Int -> Integer -> Integer
iter' bound expSum best
    | trace ("expSum = " ++ (show expSum) ++ 
             ", best = " ++ (show best)) False = 0
    | best <= 2^expSum   = best
    | otherwise          = iter' bound (expSum+1) (min best best')
  where
    ess = rejectBelow bound $ genExpoSeq expSum
    best' = minimum $ map exponentsToNumber ess


-- exponentsToNumber returns the number corresponding to a sequence of prime
-- exponents
exponentsToNumber :: [Int] -> Integer
exponentsToNumber = product . zipWith (^) primes

main = do
    putStrLn $ "1000:   " ++ show (iter 1000)
    putStrLn $ "4x10^6: " ++ show (iter (4*10^6))