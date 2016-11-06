import Math.NumberTheory.Primes.Sieve(primes)
import Data.List(sort)
import Debug.Trace(trace)

-- Generate the polygonal number
polygonalNumber :: Int -> Int -> Int
polygonalNumber b n = n*((b-2)*n+(4-b)) `div` 2


-- Returns the number of decimal digits of a number
nbrDigits :: Integral a => a -> Int
nbrDigits = (+1) . floor . logBase 10 . fromIntegral


-- Returns the number obtained by concating two numbers one after the other
(#) :: Integral a => a -> a -> a
n1 # n2 = n1 * 10^nbrDigits n2 + n2


-- tools
isEmpty [] = True
isEmpty _  = False

isSingleton [_] = True
isSingleton _ = False

isPerfectSquare :: Int -> Bool
isPerfectSquare n2 = (n*n == n2) where n = round $ sqrt $ fromIntegral n2


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

{-# LANGUAGE BangPatterns #-}
import System.CPUTime
timeIt :: (a -> String) -> a -> IO ()
timeIt toString exp = do
    t1 <- getCPUTime
    let !exp' = exp
    t2 <- getCPUTime
    putStrLn $ (show $ round $ fromIntegral (t2-t1) / 10^9) ++ " \t| " ++ toString exp'


--------------------------------------------------------------------------------
-- Simple continued fractions (i.e. expressed in cannonical form)
--------------------------------------------------------------------------------

-- Compute successive approximations of a continued fraction given its
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


-- Return the coefficients of the continued fraction equals to the square root
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

-- The coefficients of the continued fraction of e
e_ContinuedFractionCoeffs :: Integral a => [a]
e_coeffs = 2 : gen 1 2
  where
    gen 1 n = 1 : gen 2 n
    gen 2 n = n : gen 3 (2+n)
    gen 3 n = 1 : gen 1 n

-- Occurence frequency
import Data.Map (fromListWith, toList)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

