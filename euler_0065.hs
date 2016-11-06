import Data.Ratio;
import Data.List(foldl1')
import Data.Char

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

e_coeffs :: Integral a => [a]
e_coeffs = 2 : gen 1 2
  where
    gen 1 n = 1 : gen 2 n
    gen 2 n = n : gen 3 (2+n)
    gen 3 n = 1 : gen 1 n

euler :: Int -> Int
euler n = sum $ map digitToInt $ show $ numerator $ (approximations e_coeffs) !! (n-1)

main = do
    print $ euler 10
    print $ euler 100
