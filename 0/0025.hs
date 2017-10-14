fib = 1 : 1 : (zipWith (+) fib (tail fib))
limit = 10^999

main = print (fst (head (dropWhile test (zip [1..] fib)))) 
          where test (i, x) = (x < limit)