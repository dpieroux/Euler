import Data.List

-- pandigitals is the list of digits d1, d2, ... d10 of all 0-to-9 pandigitals 
-- numbers that verify 
--      - d1 /= 0
--      - d2d3d4 is even
--      - d4d5d6 is a multiple of 5

pandigitals :: [[Int]]
pandigitals = perm 1 [0..9] where
    perm :: Int -> [Int] -> [[Int]]
    perm n ls 
        | null ls   = [[]]
        | otherwise = perm' n ls ls 
        where         
            perm' n' [] bs = []
            perm' n' (a:as) bs 
                | (n' == 1) && (a == 0)             = perm'' -- skip if d1 is zero
                | (n' == 4) && (odd a)              = perm'' -- skip if d4 is odd
                | (n' == 6) && (a /= 0) && (a /= 5) = perm'' -- skip if d6 is not 0 or 5
                | otherwise = map (a:) (perm (n'+1) $ delete a bs) ++ perm''
                where 
                    perm'' = perm' n' as bs

-- Given the digits [d1, ..., d10] of a 0-to-9 pandigital, check that d3d4d5, 
-- d5d6d7, d6d7d8, d7d8d9 and d8d9d10 are respectively multiple of 3, 7, 11, 13
-- and 17.
hasProperty :: [Int] -> Bool
hasProperty digits =
    (rem (dropAndValue 2 digits) 3 == 0) && 
    (rem (dropAndValue 4 digits) 7 == 0) && 
    (rem (dropAndValue 5 digits) 11 == 0) && 
    (rem (dropAndValue 6 digits) 13 == 0) && 
    (rem (dropAndValue 7 digits) 17 == 0)
    where
        dropAndValue :: Int -> [Int] -> Int
        dropAndValue n = value . drop n
        value (a:b:c:ds) = (((a*10)+b)*10+c)

-- Given a list of digits, compute the corresponding value. Note the the first
-- digit must be the least significant.
value :: [Int] -> Int
value [] = 0
value (n:ns) = n + 10 * (value ns)

main = do 
    print $ sum $ map (value . reverse) $ filter hasProperty pandigitals