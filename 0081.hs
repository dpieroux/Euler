import Data.Array

euler :: Array (Int, Int) Int -> Int
euler cost = path!(i1, j1)
  where
    ((i0, j0), (i1, j1)) = bounds cost
    path = array (bounds cost)
        (   ((i0, j0), cost!(i0, j0))
        :   [((i0, j), cost!(i0, j) + path!(i0, j-1)) | j <- [j0+1 .. j1]]
        ++  [((i, j0), cost!(i, j0) + path!(i-1, j0)) | i <- [i0+1 .. i1]]
        ++  [((i, j), cost!(i, j) + ((path!(i, j-1)) `min` (path!(i-1, j)))) | i <- [i0+1 .. i1], j <- [j0+1 .. j1]])

sampleCost :: Array (Int, Int) Int
sampleCost = listArray ((1,1), (5,5))
    [   131, 673, 234, 103, 18,
        201,  96, 342, 965, 150,
        630, 803, 746, 422, 111,
        537, 699, 497, 121, 956,
        805, 732, 524,  37, 331]

main = do
    src <- readFile "z:/euler/euler_0083.dat" -- Same file as for problem 83
    let rows = map (\r -> (read ("[" ++ r ++ "]"))::[Int]) $ lines src
    let i1 = length rows
    let j1 = length (head rows)
    let cost = listArray ((1, 1), (i1, j1)) $ concat rows
    putStrLn  $ "test: " ++ show (euler sampleCost)
    putStrLn  $ "actual: " ++ show (euler cost)
