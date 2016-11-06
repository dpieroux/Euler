import Data.Array.Unboxed

improve :: Array (Int, Int) Int -> Array (Int, Int) Int -> Array (Int, Int) Int
improve path cost = array (bounds path)
    (   ((i0, j0), cost!(i0, j0))
    :   ((i0, j1), path!(i0, j1) `min` (cost!(i0,j1) + minTopRight))
    :   ((i1, j0), path!(i1, j0) `min` (cost!(i1,j0) + minBottomLeft))
    :   ((i1, j1), path!(i1, j1) `min` (cost!(i1,j1) + minBottomRight))
    :   [((i,j),  path!(i,j)  `min` (cost!(i,j)  + minCenter i j)) | i <- [i0+1..i1-1], j <- [j0+1..j1-1]]
    ++  [((i0,j), path!(i0,j) `min` (cost!(i0,j) + minTop j))      | j <- [j0+1..j1-1]]
    ++  [((i1,j), path!(i1,j) `min` (cost!(i1,j) + minBottom j))   | j <- [j0+1..j1-1]]
    ++  [((i,j0), path!(i,j0) `min` (cost!(i,j0) + minLeft i))     | i <- [i0+1..i1-1]]
    ++  [((i,j1), path!(i,j1) `min` (cost!(i,j1) + minRight i))    | i <- [i0+1..i1-1]]
    )
  where
    ((i0, j0), (i1, j1)) = bounds path
    minCenter i j  = (path!(i-1,j)) `min` (path!(i+1,j)) `min` (path!(i,j-1)) `min` (path!(i,j+1))
    minTop      j  = (path!(i0+1,j)) `min` (path!(i0,j-1)) `min` (path!(i0,j+1))
    minBottom   j  = (path!(i1-1,j)) `min` (path!(i1,j-1)) `min` (path!(i1,j+1))
    minLeft   i    = (path!(i+1,j0)) `min` (path!(i-1,j0)) `min` (path!(i,j0+1))
    minRight  i    = (path!(i+1,j1)) `min`  (path!(i-1,j1)) `min` (path!(i,j1-1))
    minTopRight    = (path!(i0+1,j1)) `min` (path!(i0,j1-1))
    minBottomLeft  = (path!(i1-1,j0)) `min` (path!(i1,j0+1))
    minBottomRight = (path!(i1-1,j1)) `min` (path!(i1,j1-1))

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

euler cost = (iter $ initPath cost) ! snd (bounds cost)
  where
    iter path = if (path == path') then path else iter path'
      where path' = improve path cost

main = do
    src <- readFile "z:/euler/euler_0083.dat"
    let rows = map (\r -> (read ("[" ++ r ++ "]"))::[Int]) $ lines src
    let i1 = length rows
    let j1 = length (head rows)
    let cost = listArray ((1, 1), (i1, j1)) $ concat rows
    putStrLn  $ "test: " ++ show (euler sampleCost)
    putStrLn  $ "actual: " ++ show (euler cost)
