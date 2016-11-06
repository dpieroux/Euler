nodes1 = [[3]
         ,[7, 4]
         ,[2, 4, 6]
         ,[8, 5, 9, 3]]

euler :: [[Int]] -> [Int]
euler = foldl update (repeat 0)
  where
    update :: [Int] -> [Int] -> [Int]
    update acc row = zipWith (+) row (zipWith max acc (tail acc))

triangulize :: String -> [[Int]]
triangulize str = map ((map read) . words) $ lines $ str

main = do
    print $ maximum $ euler $ reverse nodes1
    ifile <- readFile "z:/euler/euler_0067.dat"
    let nodes2 = triangulize ifile
    print $ maximum $ euler $ reverse nodes2