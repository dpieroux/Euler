import Math.NumberTheory.Primes.Factorisation(factorise)
import Data.List(foldl')

triangles_numbers = 1 : zipWith (+) triangles_numbers [2..]

get_nbr_divisors n = foldl' (\acc (p, e) -> acc*(e+1)) 1 $ factorise n

euler minBound = filter (\n -> minBound < get_nbr_divisors n) triangles_numbers

main = do
    print $ head $ euler 5
    print $ head $ euler 500
