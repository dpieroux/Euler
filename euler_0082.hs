import Data.Array.Unboxed

improve :: Array (Int, Int) Int -> Array (Int, Int) Int -> Array (Int, Int) Int
improve path cost = array (bounds path)
    (   [((i , j0), cost!(i, j0))                                       | i <- [i0..i1]]
    ++  [((i0, j) , path!(i0, j) `min` (cost!(i0, j) + minTop    j))    |                  j <- [j0+1..j1]]
    ++  [((i , j) , path!(i, j)  `min` (cost!(i , j) + minCenter i j))  | i <- [i0+1..i1], j <- [j0+1..j1]]
    ++  [((i1, j) , path!(i1, j) `min` (cost!(i1, j) + minBottom j))    |                  j <- [j0+1..j1]]
    )
  where
    ((i0, j0), (i1, j1)) = bounds path
    minCenter i j  = (path!(i -1, j)) `min` (path!(i +1, j)) `min` (path!(i , j-1))
    minTop      j  =                        (path!(i0+1, j)) `min` (path!(i0, j-1))
    minBottom   j  = (path!(i1-1, j))                        `min` (path!(i1, j-1))

initPath :: Array (Int, Int) Int -> Array (Int, Int) Int
initPath cost = listArray (bounds cost) $ repeat maxItem
  where
    items = elems cost
    maxItem = length items * maximum items

sampleCost :: Array (Int, Int) Int
sampleCost = listArray ((1,1), (5,5))
    [   131, 673, 234, 103, 18,
        201,  96, 342, 965, 150,
        630, 803, 746, 422, 111,
        537, 699, 497, 121, 956,
        805, 732, 524,  37, 331]

euler cost = minimum [converged_cost!(i, j1) | i <- [i0..i1]]
  where
    converged_cost = iter $ initPath cost
    iter path = if (path == path') then path else iter path'
      where path' = improve path cost
    ((i0, j0), (i1, j1)) = bounds converged_cost


main = do
    src <- readFile "z:/euler/euler_0083.dat" -- same file as problem 83
    let rows = map (\r -> (read ("[" ++ r ++ "]"))::[Int]) $ lines src
    let i1 = length rows
    let j1 = length (head rows)
    let cost = listArray ((1, 1), (i1, j1)) $ concat rows
    putStrLn  $ "test: " ++ show (euler sampleCost)
    putStrLn  $ "actual: " ++ show (euler cost)
