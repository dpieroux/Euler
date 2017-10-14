import Data.List (tails, foldl')
import Data.List.Ordered(sortOn)
import Math.NumberTheory.Primes (primes, isPrime)

type Prime = Integer

euler_50 limit = foldl' update (0, 0) $ tails $ takeWhile (<limit) primes
  where
    update :: (Prime, Int) -> [Prime] -> (Prime, Int)
    update best ps 
        | snd(best)>snd(cur) = best 
        | otherwise          = cur 
      where 
        cur = iter (0, 0) (0, 0) ps

    iter :: (Prime, Int) -> (Prime, Int) -> [Prime] -> (Prime, Int)
    iter best@(sum, n) cur@(sum', n') (p:ps)
        | limit <= sum''            = best
        | isPrime sum'' &&  n < n'' = iter next next ps
        | otherwise                 = iter best next ps        
      where 
        next@(sum'', n'') = (sum' + p, n' + 1)
    iter best _ [] = best

main = do
    print $ euler_50 (10^2)
    print $ euler_50 (10^3)
    print $ euler_50 (10^6)

