import Data.List
import Math.NumberTheory.Primes 

-- Turn a number into its digits 
digits 0 = []
digits n = d : digits n' where (n', d) = divMod n 10

-- Turn a list of digits into a number. Invariant: undigits . digits = id
undigits = foldr (\d acc -> d + 10 * acc) 0

-- Given a list, return a list of the rotations of its elements
rotate ls = tail $ zipWith (++) (tails ls) (inits ls)

-- Given a list of digits of a prime, check that the rotated numbers are primes
isCircularPrime ds = all isPrime $ map undigits $ init $ rotate ds

-- Only numbers made of the following digits might be circular primes, excepted 2 and 5
goodDigits = [1, 3, 7, 9]

-- Return the circular primes smaller than the parameter
circularPrimesBelow n = 2 : 5 : (filter filterP $ takeWhile (<n) primes) where
    filterP n = all (\d -> elem d goodDigits) ds && isCircularPrime ds where
        ds = digits n

-- Main
main = do
    print $ length $ circularPrimesBelow $ 10^6