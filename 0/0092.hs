import qualified Data.Map.Strict as Map

-- The method here is brute force with Memoization. It runs in about 20s on my
-- office laptop.
--
-- However, there is a much better method: since the order of the digits is
-- irrelevant, there is no need to test all numbers up to 10^7, but only those
-- with, let say, increasing digits. In addition, the addition of the square of
-- the digits of numbers below 10^7 is not greater than 7x9² = 567. So no need
-- to remember the answer for higher numbers. (See the forum)
--

op :: Integer -> Integer
op 0 = 0
op n = r^2 + op q
  where
    (q, r) = quotRem n 10


doesReach89 :: Integer -> Map.Map Integer Bool -> (Bool, Map.Map Integer Bool)
doesReach89 n mem = case Map.lookup n mem of
    Just b  -> (b, mem)
    Nothing -> (ans, Map.insert n ans mem')
  where
    (ans, mem') = doesReach89 (op n) mem

memInit = Map.fromList [(1, False), (89, True)]

euler :: Integer -> Int
euler up_bound = euler' 1 memInit 0
  where
    euler' :: Integer -> Map.Map Integer Bool -> Int -> Int
    euler' i mem acc
        | i == up_bound = acc
        | hasReach89    = euler' (i+1) mem' (acc+1)
        | otherwise     = euler' (i+1) mem' acc
      where
        (hasReach89, mem') = doesReach89 i mem

main = print $ euler (10^7)