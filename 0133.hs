{-------------------------------------------------------------------------------

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k; for example, R(6) = 111111.

Let us consider repunits of the form R(10ⁿ).

Although R(10), R(100), or R(1000) are not divisible by 17, R(10000) is the
divisible by 17. Yet there is no value of n for which R(10ⁿ) will divide by 19.
In fact, it is remarkable that 11, 17, 41, and 73 are the only four primes below
one-hundred that can be a factor of R(10ⁿ).

Find the sum of all the primes below one-hundred thousand that will never be a
factor of R(10ⁿ).

--------------------------------------------------------------------------------

Let given n, let k(n) be the number of digit of the smaller repunit divisible by
n. See problem 129 over how to compute k(n).

Given a number p, p divides R(10ⁿ) iif k(p) divides 10ⁿ. Since 10ⁿ = 2ⁿ 5ⁿ, then
p divides R(10ⁿ) iif k(p) has only 2 and 5 as prime factors. So, any p such that
k(p) has prime factors other than 2 and 5 fulfils the condition.

-------------------------------------------------------------------------------}

import qualified Data.Numbers.Primes as Primes
import Data.Array.Unboxed
import Data.Array.IArray

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

-- Given n, returns the smallest k such that n|R(k).
k :: Int -> Int
k n = k' (n*(targetToDigit ! 1) `quot` 10) 1
  where
    n0 = rem n 10
    targetToDigit = gDigitTargetToDigit ! n0

    k' :: Int -> Int -> Int
    k' rest acc
        = if (rest == 0 && acc /= 0)
            then acc
            else let d = targetToDigit ! (gRestToTarget ! (rem rest 10))
                 in k' ((n*d + rest) `quot` 10) (acc+1)

-- Given n, returns true if n has other prime factors then 2 and 5
hasRightFactors :: Int -> Bool
hasRightFactors n = let (q, r) = quotRem n 5
            in if r==0 then hasRightFactors q else hasRightFactors2 n
  where
    hasRightFactors2 1 = False
    hasRightFactors2 n = if even n then hasRightFactors2 (quot n 2) else True

euler bound = 7 -- 2 and 5 divide no repunit
            + (sum $ [p | p <- takeWhile (<bound) (3 : drop 3 Primes.primes)
                        , hasRightFactors $ k p])

main = do
    putStrLn $ "Euler 133: " ++ (show $ euler (10^5))