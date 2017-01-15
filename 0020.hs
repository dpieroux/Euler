import Data.Char (digitToInt)

factorials = 1 : zipWith (*) factorials [2..]

euler i = sum $ map digitToInt $ show $ factorials !! (i-1)

main = do
    print $ euler 10
    print $ euler 100