{-------------------------------------------------------------------------------

There are some prime values, p, for which there exists a positive integer, n,
such that the expression n³ + n²p is a perfect cube.

For example, when p = 19, 8³ + 8²×19 = 12³.

What is perhaps most surprising is that for each prime with this property the
value of n is unique, and there are only four such primes below one-hundred.

How many primes below one million have this remarkable property?

--------------------------------------------------------------------------------

The problem to solve is to count the number of primes p<10⁶ such that it exists
n and m such that n³ + p n² = m³.

Obviously m>n. Thus let pose m = n + λ, with λ>0. It then comes
    (p-3λ) n² = 3 λ² n + λ³

Because 3 λ² n + λ³ > 0, a necessary condition for a solution is 3λ < p.

The solutions for n are:

          λ 3λ ± δ
    n± =  - ------ with δ² = (4p-3λ)λ
          2  p-3λ

3λ < p implies that
    1. δ² is positive and therefore δ is real;
    2. 3λ < δ and therefore n₋ is negative, which is not acceptable.

So the algorithm is as follows:
    For each prime below 10⁶:
        for each λ in [1 p/3[
            if δ² is a perfect square
                then if 2(p-3λ) divise λ(3λ+δ)
                    then a candidate is found. Because it is unique, we can consider directly the next prime

-------------------------------------------------------------------------------}

import qualified Data.Numbers.Primes as Primes

squares = [(n, n^2) | n <- [1..]]


-- Given a list of pairs (λ, δ²), returns the list of pair (λ, δ) for δs which
-- are perfect squares
selectAndReduce :: [(Int, Int)] -> [(Int, Int)]
selectAndReduce ((λ, δ2):pairs)
  = let δ = floor $ sqrt $ fromIntegral $ δ2 + 1
        rest = selectAndReduce pairs
    in
        if δ*δ == δ2 then (λ, δ) : rest else rest

selectAndReduce [] = []

-- Check if the given prime allows for a solution to the equation
isValidPrime p = not $ null solution
  where
    λs = [1.. div (p-1) 3]
    candidates = selectAndReduce [(λ, (4*p-3*λ)*λ) | λ <- λs]
    solution = filter (\(λ, δ) -> 0 == rem (λ*(3*λ+δ)) (2*(p-3*λ))) candidates
-- Note that because of the use of null and laziness, the filtering in
-- computing 'solution' will stop as soon as a solution is found

euler :: Int -> Int
euler upperBound = recurse (takeWhile (<upperBound) Primes.primes) 0
  where
    recurse (p:ps) acc
      | isValidPrime p = recurse ps acc'
      | otherwise      = recurse ps acc
      where
        acc' = let acc' = acc+1 in seq acc' acc'
    recurse [] acc = acc

main = do
    putStrLn $ "Euler 131: " ++ show(euler $ 10^6)