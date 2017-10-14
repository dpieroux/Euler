import Data.List(foldl')
import Math.NumberTheory.Primes.Sieve (primes)

-- The result is given by mutliplying the highest exponent of each prime not
-- greater than the range upper bound
euler ubound = foldl' (*) 1 $ map (greatestFactor 1) $ takeWhile (<=ubound) primes
  where
    greatestFactor acc n
        = let acc' = n*acc in
            if ubound < acc' then acc else greatestFactor acc' n

main = do
    print $ euler 10
    print $ euler 20