import Data.List(permutations, sort, nub)

polygon :: Int -> Int -> Int
polygon b n = n*((b-2)*n+(4-b)) `div` 2

setOfBase b = takeWhile (<=9999) $ dropWhile (<1000) $ map (polygon b) [1..]

type SplitNumber = (Int, Int)

sets :: [[SplitNumber]]
sets = map (map split . setOfBase) [3 .. 8]
  where
    split :: Int -> SplitNumber
    split n = quotRem n 100

reducePair :: ([SplitNumber], [SplitNumber]) -> ([SplitNumber], [SplitNumber])
reducePair x@(as, bs) = if x==x' then x else reducePair x'
  where
    beginning_bs = map fst bs
    as' = filter (\a -> elem (snd a) beginning_bs) as
    ending_as' = map snd as'
    bs' = filter (\b -> elem (fst b) ending_as') bs
    x' = (as', bs')

reduceSet :: [[SplitNumber]] -> [[SplitNumber]]
reduceSet ls@[a, b, c, d, e, f]
    | or $ map isEmpty ls = []
    | and $ map isSingleton ls = if isSingleton b' then ls else []
    | otherwise = reduceSet [b', c, d, e, f, a']
  where
    (a', b') = reducePair (a, b)

isEmpty [] = True
isEmpty _  = False

isSingleton [_] = True
isSingleton _ = False

euler = sum $ map (\(x, y) -> x*100+y) $ head $ filter (not.isEmpty) $ map concat $ map reduceSet $ permutations sets

main = print euler