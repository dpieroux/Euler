import Data.List.Split
import Data.Char

isTriangleNumber :: Int -> Bool
isTriangleNumber t = 2*t == n*(n+1) where n = round ((sqrt (fromIntegral (1 + 8*t))-1)/2)

isTriangleWord :: String -> Bool
isTriangleWord = isTriangleNumber . sum . map (\c -> ord c - 64)

euler42 :: [String] -> Int
euler42 = length . filter isTriangleWord

main = do
    input <- readFile "y:/euler_42.dat"
    let words = splitOn "," $ filter (\c -> c /= '"') input
    print $ euler42 words