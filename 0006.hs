euler n = (\ns -> (sum ns)^2 - sum (map (^2) ns)) [1..n]

main = do
    print $ euler 10
    print $ euler 100