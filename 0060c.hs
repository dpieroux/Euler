import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.Time.Clock

-- Returns the number of decimal digits of a number
nbrDigits :: Integral a => a -> Int
nbrDigits = (+1) . floor . logBase 10 . fromIntegral


-- Returns the number obtained by concating two numbers one after the other
(#) :: Integral a => a -> a -> a
n1 # n2 = n1 * 10^nbrDigits n2 + n2


-- Check if a pair of numbers is valid
isValidPair :: Integer -> Integer -> Bool
isValidPair n1 n2 = isPrime (n1#n2) && isPrime (n2#n1)


type ExtendedSet = ([Integer], Integer)


-- The list of all valid sets.
validSets :: [[Integer]]
validSets = [] : [2] : [3] : merge (drop 2 set1) (drop 2 set2)
  where
    set1, set2 :: [[Integer]]
    set1 = [[], [3]] ++ (generate set1 $ filter (\n -> rem n 3 == 1) (drop 2 primes))
    set2 = [[], [3]] ++ (generate set2 $ filter (\n -> rem n 3 == 2) (drop 2 primes))

    generate :: [[Integer]] -> [Integer] -> [[Integer]]
    generate ([]:vsets) primes = [head primes] : generate vsets primes
    generate (vset:vsets) (p:ps)
        | vset == [p]   = generate validSets ps
        | isNewValidSet = (p:vset) : generate vsets (p:ps)
        | otherwise     = generate vsets (p:ps)
      where
        isNewValidSet = and $ map (isValidPair p) vset

    merge :: [[Integer]] -> [[Integer]] -> [[Integer]]
    merge lss@(ls:lss') rss@(rs:rss')
        | head ls < head rs = ls : merge lss' rss
        | otherwise         = rs : merge lss  rss'

-- This function doesn't ensure that the answer corresponds to the minimal sum
euler :: Int -> ([Integer], Integer)
euler size = (head $ candidates, upBound)
  where
    candidates :: [[Integer]]
    candidates = filter (\ls -> size == length ls) validSets
    -- All the valid sets with the required number of elements

    upBound :: Integer
    upBound = sum $ head candidates
    -- The sum of the first candidate is de facto an upper bound

    candidates' :: [[Integer]]
    candidates' = takeWhile (\set -> head set < upBound) candidates
    -- Refine the candidates by considering only those whose greatest elements
    -- (i.e. ) the first is smaller than the

    candidates'' :: [ExtendedSet]
    candidates'' = zip candidates' $ map sum candidates'
    -- Zip each refined candidate with its sum

    pickupBest :: ExtendedSet -> [ExtendedSet] -> ExtendedSet
    pickupBest (cur, sum) [] = (cur, sum)
    pickupBest (cur, sum) ((cur', sum'):candidates')
        | sum'<sum  = pickupBest (cur', sum') candidates'
        | otherwise = pickupBest (cur , sum ) candidates'

main = do
    t1 <- getCurrentTime
    putStr $ "3: " ++ (show $ euler 3) ++ " => "
    t2 <- getCurrentTime
    print $ (diffUTCTime t2 t1)

    t1 <- getCurrentTime
    putStr $ "4: " ++ (show $ euler 4) ++ " => "
    t2 <- getCurrentTime
    print $ (diffUTCTime t2 t1)

    t1 <- getCurrentTime
    putStr $ "5: " ++ (show $ euler 5) ++ " => "
    t2 <- getCurrentTime
    print $ (diffUTCTime t2 t1)

    t1 <- getCurrentTime
    putStr $ "6: " ++ (show $ euler 6) ++ " => "
    t2 <- getCurrentTime
    print $ (diffUTCTime t2 t1)

{-
Y:\>euler_0060b.exe
3: ([67,37,3],107) => 0.0110006s
4: ([673,109,7,3],792) => 0.0780045s
-}