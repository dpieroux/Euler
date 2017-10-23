{-------------------------------------------------------------------------------

Consider the consecutive primes p₁ = 19 and p₂ = 23. It can be verified that 1219 is the smallest number such that the last digits are formed by p₁ whilst also being divisible by p₂.

In fact, with the exception of p₁ = 3 and p₂ = 5, for every pair of consecutive primes, p₂ > p₁, there exist values of n for which the last digits are formed by p₁ and n is divisible by p₂. Let S be the smallest of these values of n.

Find ∑ S for every pair of consecutive primes with 5 ≤ p₁ ≤ 1000000.

-------------------------------------------------------------------------------}

import qualified Data.Numbers.Primes as Primes
import qualified Data.Digits as Digits

unDigits = Digits.unDigits 10

from0to9 = [0..9]

-- Given m and e, build the smallest number S such that m|S and S ends by e.
genS :: Int -> Int -> Int
genS m e =  (m *) $ minimum $ recGenS e 0 [] []
  where
    recGenS :: Int -> Int -> [Int] -> [Int] -> [Int]
    recGenS   0    _ digits acc = unDigits digits : acc
    recGenS end rest digits acc =
        concat $ map (\(d, r) -> recGenS end' r (d:digits) acc) ds
      where
        (end', end'') = quotRem end 10
        ds = [(d, r') | d <- from0to9
                      , let (r', r0') = quotRem (m * d + rest) 10
                      , r0' == end'']

-- return ∑ S for every pair of consecutive primes with 5 ≤ p₁ ≤ bound
euler bound =
    sum $ map (\(p1, p2) -> genS p2 p1)
    $ takeWhile (\(p1, p2) -> p1 <= bound)
    $ dropWhile (\(p1, p2) -> p1 < 5)
    $ zip Primes.primes $ tail Primes.primes

main = do
    putStr "Smallest number ending by 19 multiple of 23: "
    putStrLn $ show $ genS 23 19
    putStrLn $ "Euler 134: " ++ (show $ euler 1000000)