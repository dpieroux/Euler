import Data.Ratio
import Math.NumberTheory.Primes.Testing(isPrime)

corners :: [[Integer]]
corners = map (\length -> [length*(length-i)+i | i<-[0..3]]) [3, 5 ..]
-- The values at the corners, for matrices of increasing size (FMIS)

nbrPrimes :: [Int]
nbrPrimes = map (length . (filter isPrime)) corners
-- Number of primes at the corners, FMIS

accPrimes :: [Int]
accPrimes = accPrimes' 0 nbrPrimes
-- Accumulated number of primes at the corners, FMIS
  where
    accPrimes' acc (n:ns) = acc' : accPrimes' acc' ns
      where acc' = acc + n

ratios = zipWith (%) accPrimes [5, 9 ..]
-- Ration of primes at the corners vs numbers of diagonal elements, FMIS

euler bound = 3 + 2 * (length $ takeWhile (bound <=) ratios)
-- The answer to the euler question for a given bound

main = do
    print $ euler 0.10