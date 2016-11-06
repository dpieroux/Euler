import Data.List
import Data.Ratio

isqrt :: Integral a => a -> a
isqrt n = if approx^2 <= n then approx else approx-1
  where
    approx = round $ sqrt $ fromIntegral n

isSquare n = n == (isqrt n)^2

-- Return the coefficients of the continous fraction equals to the square root
-- of the parameters. For perfect square, only the first parameter is returned.
sqrtCoeffs :: Integral a => a -> [a]
sqrtCoeffs n = if (isqrtn^2 == n) then [isqrtn] else (isqrtn : iter isqrtn 1)
  where
    isqrtn = isqrt n
    iter a b = d : iter a' b'

      where
        b' = (n-a^2) `div` b
        d  = (isqrtn + a) `div` b'
        a' = d * b' - a

-- Compute successive approximations of a continuous fraction given its
-- successive coefficients. If the coefficients are [a, b, c...], then the
-- approximations are: a, a+(1/b), a+(1/b+(1/c)), ...
approximations :: Integral a => [a] -> [Ratio a]
approximations (a:b:cs) = res
  where
    res = (a % 1) : ((a*b+1)%b) : zipWith3 combine res (tail res) cs
    combine a b c =
         (c * numerator b + numerator a) % (c * denominator b + denominator a)
approximations [a] = repeat (a%1)
approximations [] = repeat (0%1)


-- The minimal value of x solution of the equation x² - d y² = 1 given d (y acts
-- as a free parameter)
min_x d = numerator $ head $ filter isSolution $ approximations $ sqrtCoeffs d
  where
    isSolution q = (numerator q)^2 == 1 + d * (denominator q)^2

euler limit = fst $ maximumBy (\a b -> compare (snd a) (snd b))
    $ map (\d -> (d, min_x d)) $ filter (not.isSquare) [1..limit]

main = do
    print $ euler 7
    print $ euler 1000
