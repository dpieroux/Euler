import Data.List (foldl1', nub)
import Data.Set (insert, empty, size)

digitize 0 acc = acc
digitize n acc = if d == 0 then empty else insert d (digitize n' acc) where (n', d) = divMod n 10

main = do 
    print $ foldl1' (+) $ nub $ (
            [a*b | a <- [1..9],   b <- [1234..(div 9876 a)], predicat (a, b)] ++ 
            [a*b | a <- [12..98], b <- [123.. (div 9876 a)], predicat (a, b)]) where
        predicat (a, b) = 9 == (size $ digitize (a*b) $ digitize b $ digitize a empty)