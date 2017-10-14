import Data.Char(ord)
import Data.List(sort)

getNames :: String -> [String]
getNames input = read $  "[" ++ input ++ "]"

charToInt c = ord c - ord '@'
wordToInt w = sum $ map charToInt w


main = do
    input <- readFile "euler_0022.dat"
    print $ sum $ zipWith (*) [1..] $ map wordToInt $ sort $ getNames input
