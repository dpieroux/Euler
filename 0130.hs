{-------------------------------------------------------------------------------

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k; for example, R(6) = 111111.

Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
there always exists a value, k, for which R(k) is divisible by n, and let A(n)
be the least such value of k; for example, A(7) = 6 and A(41) = 5.

You are given that for all primes, p > 5, that p − 1 is divisible by A(p). For
example, when p = 41, A(41) = 5, and 40 is divisible by 5.

However, there are rare composite values for which this is also true; the first
five examples being 91, 259, 451, 481, and 703.

Find the sum of the first twenty-five composite values of n for which GCD(n, 10)
= 1 and n − 1 is divisible by A(n).

--------------------------------------------------------------------------------

The algorithm is to get the list of all composite numbers, to compute A(n) for
them and to check if the condition is fulfilled.

For the computation of A(n), see the problem 129.

-------------------------------------------------------------------------------}

import Data.Array.Unboxed
import Data.Array.IArray
import Data.Numbers.Primes

type Rest = Int
type Digit = Int

-- Given a rest r, gRestToTarget returns the digit that, when added to r returns
-- either 1 (if r=0 and 1) or 11 (otherwise).
gRestToTarget :: UArray Rest Digit
gRestToTarget = listArray (0, 9) [rem (11 - i) 10 | i <- [0..9]]


-- Given the last digit of n and the rest r from a previous step, returns the
-- next digit of d.
gDigitTargetToDigit :: Array Digit (UArray Rest Digit)
gDigitTargetToDigit = array (0,9)
    [   (1, listArray (0, 9) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    ,   (3, listArray (0, 9) [0, 7, 4, 1, 8, 5, 2, 9, 6, 3])
    ,   (7, listArray (0, 9) [0, 3, 6, 9, 2, 5, 8, 1, 4, 7])
    ,   (9, listArray (0, 9) [0, 9, 8, 7, 6, 5, 4, 3, 2, 1])
    ]

computeA :: Int -> Int
computeA n = computeA' (n*(targetToDigit ! 1) `quot` 10) 1
  where
    n0 = rem n 10
    targetToDigit = gDigitTargetToDigit ! n0

    computeA' :: Int -> Int -> Int
    computeA' rest acc
        = if (rest == 0 && acc /= 0)
            then acc
            else let d = targetToDigit ! (gRestToTarget ! (rem rest 10))
                 in computeA' ((n*d + rest) `quot` 10) (acc+1)

-- IsValid returns true if a number fulfil the condition of the exercise
isValid :: Int -> Bool
isValid n = rem (n-1) (computeA n) == 0

-- List of the number fulfilling the conditions

type Prime = Int
computeValues :: [Int] -> [Prime] -> [Int]

computeValues (n:ns') ps@(p:ps')
  | n == p    = computeValues ns' ps' -- skip primes
  | isValid n = n : computeValues ns' ps
  | otherwise = computeValues ns' ps

values = computeValues [n | i <- [1..], let n=2*i+1, rem n 5 > 0]
                       $ dropWhile (\n -> n<7) primes

main = do
    putStrLn $ "First 5 values: " ++ (show $ take 5 values)
    putStrLn $ "sum of first 25 values: " ++ (show $ sum $ take 25 values)