main = do
    print $ sum $ filter (\n -> f n == n) [2 .. 6*9^5] where
        f 0 = 0
        f n = d^5 + f n' where (n', d) = divMod n 10
