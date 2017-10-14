fib = 1 : 1 : zipWith (+) fib (tail fib)

main = print $ sum $ takeWhile (<4000001) $ filter even fib