import Data.Dates

sumProperDivisors :: Int -> Int
sumProperDivisors n = iter 2 1
  where
    iter i acc
        | isDivisor && (i*i == n) = acc + i
        | isDivisor               = iter (i+1) (acc + i + n `div` i)
        | i*i >= n                = acc
        | otherwise               = iter (i+1) acc
      where
        isDivisor = n `rem`i == 0

amicaleNumbers :: Int -> [Int]
amicaleNumbers bound = filter predicate [1..bound]
  where
    predicate n =  n /= conjugate && n == sumProperDivisors conjugate
      where
        conjugate = sumProperDivisors n

main = print $ sum $ amicaleNumbers 9999