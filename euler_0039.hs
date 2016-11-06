import Data.List (nub, foldl')
import qualified Data.Map.Strict as Map

-- Pairs (small side, perimeter) of all square triangles whose perimeter is not greater than 1000 
triangles :: [(Int, Int)]
triangles = nub [(l*min a b, l*p) | m <- [1..16], 
                                 n <- [1..m-1], 
                                 let p = 2*m*(m+n), 
                                 let a = m*m-n*n, 
                                 let b = 2*m*n, 
                                 l <- [1 .. div 1000 p]]

-- Map whose key are the perimeters and values the number of triangles with that perimeter
scores :: Map.Map Int Int 
scores = foldl' (\map (a, p) -> Map.insertWith (+) p 1 map) Map.empty triangles


-- The pair (perimeter, score) with the highest score
maxItem :: (Int, Int)
maxItem = Map.foldlWithKey' bestBetween (0, 0) scores where
    bestBetween current@(_, n) k' n' = if n>n' then current else (k', n')

main = do
    print $ maxItem