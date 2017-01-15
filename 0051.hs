import Math.NumberTheory.Primes.Sieve(primes)
import Math.NumberTheory.Primes.Testing(isPrime)
import Data.Time.Clock

type Index = Int
type Prime = Integer

--------------------------------------------------------------------------------
-- Given an association list, group togther all the values associated to a same
-- key.
--
-- Ex: groupValues [('a', 1), ('b', 2), ('a', 3)] -> [[1,3],[2]]
--------------------------------------------------------------------------------

groupValues :: Eq k => [(k, v)] -> [[v]]
groupValues ((key, value):ls) = keys : groupValues rest
  where
    (keys, rest) = groupForKey ls [value] []

    -- Groupe all values corresponding to the key above
    groupForKey (cur@(key', value'):ls) keys rest
        | key == key' = groupForKey ls (value':keys)      rest
        | otherwise   = groupForKey ls         keys  (cur:rest)
    groupForKey [] keys rest = (reverse keys, reverse rest)

groupValues [] = []


--------------------------------------------------------------------------------
-- Return all the lists obtained by removing 0, any one, any two, ... any n
-- elements from the given list. The minLen parameter is the length of the
-- smallest lists to consider.
--
-- Ex: shortenList 3 "abcd" -> ["abcd","bcd","acd","abd","abc"]
--------------------------------------------------------------------------------

shortenList :: Int -> [a] -> [[a]]
shortenList minLen ls = reduce (length ls) ls
  where
    reduce :: Int -> [a] -> [[a]]
    reduce len ls
        | len < minLen = []
        | otherwise   = ls : (concat $ map (reduce (len-1)) (reduce1 ls))

    -- Return all the lists obtained by removing one element to the parameter
    -- list
    reduce1 (l:ls) = ls : map (l:) (reduce1 ls)
    reduce1 []     = []


--------------------------------------------------------------------------------
-- Return the given list with all elements located at the given indices replaced
-- by a same element.
--
-- Ex: substAtIndices [1,3] '*' "abcde" -> "a*c*e"
--------------------------------------------------------------------------------

substAtIndices :: [Index] -> a -> [a] -> [a]
substAtIndices indices elem ls = subst 0 indices ls
  where
    subst i indices@(index:indices') (l:ls')
        | i == index = elem : subst (i+1) indices' ls'
        | otherwise  =    l : subst (i+1) indices  ls'
    subst _ [] ls = ls
    subst _  _ [] = []


--------------------------------------------------------------------------------
-- Check if the replacement of chosen digits of a given prime allows generating
-- a sequence of primes of the given len
--------------------------------------------------------------------------------

predicate :: Int -> Prime -> Bool
predicate len prime = any predicate' indicesSets
  where
    predicate' :: [Int] -> Bool
    predicate' indices
        = (len==) . length . filter isPrime
        $ map replaceDigitsBy [(primes' !! head indices) .. '9']
      where
        replaceDigitsBy d = read $ substAtIndices indices d primes'

    primes' = show prime
    -- Decimal representation of the given prime

    biggestDigit = head $ show (10-len)
    -- Biggest digit candidate for replacement. Above, it is not possible to
    -- to reach the length required.

    indicesSets
    -- List of all groups of digits candidates for replacement.
        = filter (\ls -> 0 == length ls `rem` 3)
        . concat . map (shortenList 3)
        . groupValues
        . filter (\(item, _) -> item <= biggestDigit)
        $ zip primes' [0..]

        {-
        From bottom to top:
            - Build a assocation list whose keys are the prime digits and the
              values are the index of the digits.
            - Only small enough digits are considered for replacement.
            - Group all the indices corresponding to the same digit.
            - Replacement could be done for 3 or more equal digits.
            - Only replacement of N digits, with N a multiple of 3, works
        -}

--------------------------------------------------------------------------------

main = do
    t1 <- getCurrentTime
    putStr $ show $ head $ filter (predicate 8) $ primes
    t2 <- getCurrentTime
    putStrLn $ " => " ++ show (diffUTCTime t2 t1)
    t1 <- getCurrentTime
    putStr $ show $ head $ filter (predicate 9) $ primes
    t2 <- getCurrentTime
    putStrLn $ " => " ++ show (diffUTCTime t2 t1)
    t1 <- getCurrentTime
    putStr $ show $ head $ filter (predicate 10) $ primes
    t2 <- getCurrentTime
    putStrLn $ " => " ++ show (diffUTCTime t2 t1)

--------------------------------------------------------------------------------

display :: Int -> Prime -> [([Int], [Prime])]
display len prime = filter (\pair -> len == length  (snd pair)) $ zip indicesSets $ map select indicesSets
  where
    select :: [Int] -> [Prime]
    select indices
        = filter isPrime
        $ map replaceDigitsBy [(primes' !! head indices) .. '9']
      where
        replaceDigitsBy d = read $ substAtIndices indices d primes'

    primes' = show prime
    -- Decimal representation of the given prime

    biggestDigit = head $ show (10-len)
    -- Biggest digit candidate for replacement. Above, it is not possible to
    -- to reach the length required.

    indicesSets
    -- List of all groups of digits candidates for replacement. 
        = filter (\ls -> 0 == length ls `rem` 3)
        . concat . map (shortenList 3)
        . groupValues
        . filter (\(item, _) -> item <= biggestDigit)
        $ zip primes' [0..]

{-
Solutions:
    8: 121313 (*2*3*3), in 0.11s
    9: 38000201 (38*0*2*1), in 56.88s
    10: 39402090703 (39402*9*7*3), in 33391.21s = 9h 16m 31.21s 
-}