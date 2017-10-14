{-
A few formulas...

p(i) = i (3 i - 1)/2  <=> i = (1 + sqrt(1 + 24 p(i)))/6

p(i) = p(i-1) + 3i -2
-}

import Data.Time.Clock

updateCandidateOver_i :: Int -> Int -> [Int] -> Int
updateCandidateOver_i candidate i pjs@(p:_)
    | candidate <= di = candidate
    | otherwise       = updateCandidateOver_i (updateCandidateOver_j pjs) (i+1) (pi:pjs)
    where
        di = 3*i-2
        pi = p + di

        updateCandidateOver_j :: [Int] -> Int
        updateCandidateOver_j (pj:pjs)
            | candidate <= dij || pj < di              = candidate
            | isPentagonal dij && isPentagonal (pi+pj) = dij
            | otherwise                                = updateCandidateOver_j pjs
            where
                dij = pi-pj


isPentagonal :: Int -> Bool
isPentagonal p = let n = round ((1+sqrt(1+24*fromIntegral p))/6) in 2*p == n*(3*n-1)


main = do
    t1 <- getCurrentTime
    print $ updateCandidateOver_i (maxBound::Int) 2 [1]
    t2 <- getCurrentTime
    print $ (diffUTCTime t2 t1)
