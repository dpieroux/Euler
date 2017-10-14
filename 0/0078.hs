-- This code uses the Euler recusrive formula of the "Partition function P".
-- See http://mathworld.wolfram.com/PartitionFunctionP.html

import Data.Array;

altern = 1 : -1 : altern;

euler :: Int -> Int -> Array Int Int
euler dim r = p
  where
    p = array (0, dim) ((0, 1):[(i, v)
        | i <- [1..dim],
          let p1s = map (p!) $ takeWhile (0<=) [i-div (k*(3*k-1)) 2 | k <- [1..]],
          let p2s = map (p!) $ takeWhile (0<=) [i-div (k*(3*k+1)) 2 | k <- [1..]],
          let p1s' = zipWith (*) p1s altern,
          let p2s' = zipWith (*) p2s altern,
          let v = mod (sum p1s' + sum p2s') r])
          -- taking the modulo at each step allows to work with Int.

main = print $ take 1 $ filter (\(i, v) -> v == 0) $ assocs $ euler 100000 (10^6)
