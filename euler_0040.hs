import Data.Char

main = do
    print $ product $ map (\index -> digitToInt $ digits !! index) indices where
        digits = concatMap show [0..]
        indices = map (10^) [0..6]
