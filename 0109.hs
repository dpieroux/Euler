import Data.List

singles = [  s | s <- [1 .. 20] ++ [25]]
doubles = [2*s | s <- [1 .. 20] ++ [25]]
triples = [3*s | s <- [1 .. 20]]

darts = sort $ 0 : (singles ++ doubles ++ triples)

-- play :: maxScore -> [[Throws]]
games target = sum $ map (games' darts) accountForFinal
  where
    accountForFinal = [target-d | d <- doubles, d <= target]


games' [] _ = 0
games' darts@(d:ds) target 
    | d >  target = 0
    | otherwise   = count (target-d) darts + games' ds target  
  where
    count target = length . takeWhile (== target) . dropWhile (<target)


euler_109 bound = sum $ map games [1..bound] 