facto n = foldr (*) 1 [1..n]
combi n r = (facto n) `div` (facto r) `div` (facto(n-r))

main = print $ length $ filter (10^6<) $ [combi n r | n <- [23..100], r <- [1..n]]