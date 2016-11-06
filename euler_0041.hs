import Data.List(permutations, sortBy)
import Math.NumberTheory.Primes(isPrime)

-- Transform a list of digits into a list of Nums.
undigits [] = 0
undigits (n:ns) = n + 10 * (undigits ns)


-- Return the list of all pandigital numbers made of n digits
pandigitals' n =  map undigits $ permutations [1..n]


-- The list of all pandigital numbers not divisible by 3, in decreasing order.
pandigitals = concat $ map ((sortBy (flip compare)).pandigitals') [7, 4, 1] where 


main = do 
    print . head $ filter isPrime $ pandigitals