import Data.Array (listArray, (!))

main = do
    print (sum $ [n | n <- [10 .. 7*(facto!9)], n == f n]) where
        f n = sum $ map (\d -> facto!d) $ digits n
        digits 0 = []
        digits n = r : (digits n') where (n', r) = divMod n 10  
        facto = listArray (0, 9) [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
