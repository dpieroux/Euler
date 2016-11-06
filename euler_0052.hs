import Data.List(sort)

--------------------------------------------------------------------------------
-- euler_52 order
--
-- Return the list of numbers n whose products by 1, 2, up to order are numbers 
-- with the same digits as n.
--------------------------------------------------------------------------------

euler_52 :: Int -> [Int]
euler_52 order = concat $ map (euler_52' order) [1..]


--------------------------------------------------------------------------------
-- euler_52' order nd
--
-- Return the list of numbers n of nd digits whose products by 1, 2, up to order
-- are numbers with the same digits as n.
--------------------------------------------------------------------------------

euler_52' :: Int -> Int -> [Int]
euler_52' order nd = filter haveSameDigits candidates
  where
    upBound = floor ((10^nd-1) / 6)
    first   = 10^(nd-1) + 2
    candidates = [first, first+3 .. upBound]

    haveSameDigits n = all (==digits) multiples
      where
        digits = sort $ show n
        multiples = map (sort . show . (*n)) [2..order]


--------------------------------------------------------------------------------

main = print $ head $ euler_52 6