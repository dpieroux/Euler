count _ [] = 0
count amount coins@(c:cs)
    | amount < 0  = 0
    | amount == 0 = 1
    | otherwise   = (count (amount-c) coins) + (count amount cs)

main = do
    print (count 200 [200, 100, 50, 20, 10, 5, 2, 1])

