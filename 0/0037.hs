import Data.List
import Math.NumberTheory.Primes 

-- Representation of a number as a list of digits. The least significant digit 
-- is first in the list.
type Digits = [Integer]


-- Turn a number into its digits. 
digitise :: Integer -> Digits
digitise n = digitise' n where
    digitise' 0 = []
    digitise' n = d : digitise' n' where (n', d) = divMod n 10


-- Turn a list of digits into a number. Invariant: undigits . digits = id
undigitise ::  Digits -> Integer
undigitise = foldr (\d acc -> 10 * acc + d) 0 


-- Return the list of primes obtained by extending the prime given in parameter 
-- by one digit to the right
growPrimeToTheRight ::  Integer -> [Integer]
growPrimeToTheRight prime = filter isPrime $ map (\d -> 10*prime + d) [1, 3, 7, 9]
       
-- List of all right truncatable primes
rightTruncatables :: [Integer]
rightTruncatables = concat $ init $ generate [[2, 3, 5, 7]] where
    generate :: [[Integer]] -> [[Integer]]
    generate acc@(seeds:_) = 
        if null seeds' then acc else generate (seeds':acc) where
            seeds' = concat $ map growPrimeToTheRight seeds

-- Check if the argument is a left truncatable prime
isLeftTruncatable :: Integer -> Bool
isLeftTruncatable = all (isPrime . undigitise) . tail . inits . digitise

main = do 
    print $ sum $ filter isLeftTruncatable rightTruncatables