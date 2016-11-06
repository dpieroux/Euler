import Data.Char

-- Return the greatest numerator a such that a² is not greater than the given
-- number n.
isqrt n =
    iter n (n+1)
  where

    iter a a'
        | a < a'    = iter (div (a*a + n) (2*a)) a
        | otherwise = fineStep a

    fineStep a
        | n < a*a         = fineStep (a-1)
        | n < (a+1)*(a+1) = a
        | otherwise       = fineStep (a+1)


-- Given the denominator b, return the greatest numerator a such that (a/b)² is
-- not greater than the given number n.

ratSqrt b n = iter (n*b) ((n*b)+1)
  where
    nb² = n*b*b

    iter a a'
        | a < a'    = iter (div (a*a + nb²) (2*a)) a
        | otherwise = fineStep a

    fineStep a
        | nb² < a*a         = fineStep (a-1)
        | nb² < (a+1)*(a+1) = a
        | otherwise         = fineStep (a+1)


euler nbrDigits n = sum $ map (digitToInt) $ take nbrDigits
    $ show $ (\a -> if (0 == mod a b) then 0 else a) $ ratSqrt b n
  where
    b = 10^nbrDigits

main = print $ sum $ map (euler 100) [1..100]