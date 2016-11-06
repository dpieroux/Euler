import Data.List

-- Representation of a number as a list of digits. The least significant digit 
-- is first in the list.
type Digits = [Int]


-- Check if all elements of a list are distinct
areDistinct :: [a] -> Bool
areDistinct [] = True
areDistinct (n:ns) = (notElem n ns) && areDistinct ns


-- Turn a number into its digits. 
digitise :: Int -> Digits
digitise 0 = []
digitise n = d : digitise n' where (n', d) = divMod n 10


-- Turn a list of digits into a number. Invariant: undigits . digits = id
undigitise ::  Digits -> Int
undigitise = foldr (\d acc -> 10 * acc + d) 0 


-- The list of potential seeds used to built the 
seeds :: [Int]
seeds = [n | n <- [12 .. 9876], 
             let digits = digitise n, 
             notElem 0 digits,
             nub digits == digits]


-- Generate the concatenated number (seed)(2 seed)...(n seed). Stop when the 
-- result has at least 9 digits.
generateFrom seed = generate' 1 [] where
    generate' n acc = if length acc < 9 
                      then generate' (n+1) (digitise (n*seed) ++ acc) 
                      else acc


-- list of all pandigital numbers built as explained in the exercise statement.
candidates = [digits | digits <- map generateFrom seeds,
                       length digits == 9,
                       notElem 0 digits,
                       areDistinct digits]


-- Print the maximum candidate
main = do 
    print $ maximum $ map undigitise candidates