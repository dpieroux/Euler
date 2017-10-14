import Data.Set (fromList, size)

main = do
    print (size $ fromList [x^y | x <- [2..100], y <- [2..100]])
