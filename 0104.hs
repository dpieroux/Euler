{-------------------------------------------------------------------------------
The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

It turns out that F541, which contains 113 digits, is the first Fibonacci number
for which the last nine digits are 1-9 pandigital (contain all the digits 1 to
9, but not necessarily in order). And F2749, which contains 575 digits, is the
first Fibonacci number for which the first nine digits are 1-9 pandigital.

Given that Fk is the first Fibonacci number for which the first nine digits AND
the last nine digits are 1-9 pandigital, find k.

-------------------------------------------------------------------------------}

import Data.List

e9 = 10^9

fibGenerator = fib 
  where 
    fib = 1 : 1 : zipWith (+) fib (tail fib)

fibGenerator' = fib 
  where 
    fib = 1 : 1 : zipWith op  fib (tail fib)
    op x y = mod (x+y) e9

pandigital = ['1'..'9']

predicate (i, n, m) = b == pandigital && a == pandigital
-- The order of the tests here is very important as computing a is slower than
-- computing b.
  where
    a = sort $ take 9 $ show n
    b = sort $ show m

main = print $ (\(i, n, m) -> i) 
             $ head 
             $ filter predicate 
             $ zip3 [1..] fibGenerator fibGenerator' 