{-------------------------------------------------------------------------------

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k.

For example, R(10) = 1111111111 = 11×41×271×9091, and the sum of these prime factors is 9414.

Find the sum of the first forty prime factors of R(10⁹).

--------------------------------------------------------------------------------

Suppose that K = n k. Then R(K) = R(k) * S(n, k-1) with S(n, k-1) the number
made of n '1' separeted bk k-1 digits '0'. For instance, for K=12, it comes:
    R(12) = 11111111111
          = R(1)  * S(12, 0) =            1 * 11111111111
          = R(2)  * S(6, 1)  =           11 * 10101010101
          = R(3)  * S(4, 2)  =          111 * 1001001001
          = R(4)  * S(3, 3)  =         1111 * 100010001
          = R(6)  * S(2, 5)  =       111111 * 1000001
          = R(12) * S(1, 11) = 111111111111 * 1

So, given a prime p (≠ 2 or 5) , we compute the smaller R(k) such that p|R(k)
(see problem 129). Then, if k|10⁹, it means that R(k)|R(10⁹), and thus p|R(10⁹).

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

-- Given n, return the prime factors of R(n).
primeFactorsR :: Int -> [Int]
primeFactorsR n = filter predicate $ 3 : dropWhile (<7) Primes.primes
  where
    predicate p = rem n (k p) == 0

euler n limit = take limit $ primeFactorsR n

main = do
    putStrLn $ "R(10), 4 first items:" ++ (show $ euler 10 4)
    putStrLn $ "R(10^9), sum of 40 first items:" ++ (show $ sum result)

  where
    result = euler (10^9) 40