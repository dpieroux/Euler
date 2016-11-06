import Math.NumberTheory.Primes.Sieve(primes)
import Data.List(sort)
import Debug.Trace(trace)

arePermutations n1 n2 = sort (show n1) == sort (show n2)

euler :: Integer -> Integer
euler end = euler' 1 0 (reverse $ takeWhile (<=div end 2) primes) [2]
  where
    euler' best_n best_phi ps@(p:pst) (q:qst)
    -- The principle of the method is to look for numbers composed of only 2 prime factors.
    -- The first prime factors form the list ps, and the seconds the list qs.
        | (n * best_phi < best_n * phi_n) && (arePermutations n phi_n)
            = euler' n phi_n ps' (dropWhile (\q -> end<(head ps')*q) ps')
        | otherwise = euler' best_n best_phi ps qst
      where
        n = p*q
        phi_n = (p-1)*(q-1)
        ps' = takeWhile (q<=) pst

    euler' best_n best_phi (_:pst) []
        | null pst = best_n
        | otherwise = euler' best_n best_phi pst $ reverse $ takeWhile (\q -> (head pst)*q <= end) primes

main = print $ euler (10^7-1)
