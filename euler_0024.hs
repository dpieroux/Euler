import Data.List

factorial :: [Int]
factorial = 1 : zipWith (*) [1..] factorial

digits :: Int -> [Int] -> [Int] 
digits 0 candidates = candidates
digits position candidates = digit : digits position' (delete digit candidates)
    where   facto = factorial !! ((length candidates) - 1)
            (index, position') = divMod position facto
            digit = candidates !! index

main = do
    print (foldl (\x y -> 10*x+y) 0 (digits 999999 [0..9]))