import Math.NumberTheory.Primes (primes, isPrime)
import Data.List (permutations, subsequences)
import Data.List.Unique (sortUniq)

--------------------------------------------------------------------------------
-- Return the sorted list of numbers obtained by permuting the digits of the 
-- function argument. 
--------------------------------------------------------------------------------

permuteDigits :: Integer -> [Integer]
permuteDigits =  sortUniq . map read . permutations . show 


--------------------------------------------------------------------------------
-- Return the list of all the triplets whose first element is the function 
-- argument and that fulfil the conditions expressed in the exercise statement.
--------------------------------------------------------------------------------

triplets :: Integer -> [[Integer]]
triplets n = filter predicat $ subsequences $ filter isPrime (permuteDigits n)
    where 
        predicat :: [Integer] -> Bool
        predicat triplet@[u, v, w] = u == n && u /= v && v-u == w-v 
        predicat _ = False

        combine :: [Integer] -> Integer
        combine = read . concat . map show 


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
main = print $ concat $ map (map combine . triplets) primesInRange 
    where
        combine :: [Integer] -> Integer
        combine = read . concat . map show 
        primesInRange = takeWhile (<=9999) $ dropWhile (<1000) primes