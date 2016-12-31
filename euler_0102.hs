{-------------------------------------------------------------------------------

Three distinct points are plotted at random on a Cartesian plane, for which
-1000 ≤ x, y ≤ 1000, such that a triangle is formed.

Consider the following two triangles:

A(-340,495), B(-153,-910), C(835,-947)

X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, whereas triangle XYZ
does not.

Using triangles.txt (right click and 'Save Link/Target As...'), a 27K text file
containing the co-ordinates of one thousand "random" triangles, find the number
of triangles for which the interior contains the origin.

NOTE: The first two examples in the file represent the triangles in the example
given above.

--------------------------------------------------------------------------------

Let A, B and C be the 3 points of a Triangle. The origin is contained in it iif
the vectorial products OA x OB, OB x OC and OC x OA are all either positive or
negative.

-------------------------------------------------------------------------------}

type Point = (Int, Int)

data Triangle = Triangle Point Point Point deriving Show

streamToInts :: String -> [Int]
streamToInts = map read
             . words 
             . map (\c -> if c==',' then ' ' else c)

intsToPoints :: [Int] -> [Point]
intsToPoints (x:y:ps) = (x, y) : intsToPoints ps
intsToPoints _ = []

pointsToTriangles :: [Point] -> [Triangle]
pointsToTriangles (p1:p2:p3:ps) = Triangle p1 p2 p3 : pointsToTriangles ps
pointsToTriangles _ = []

-- Vectorial products: A -> B -> (OA x OB)
(.*.) :: Point -> Point -> Int
(px, py) .*. (qx, qy) = px*qy - qx*py

originInside :: Triangle -> Bool
originInside (Triangle a b c) = let ca = c .*. a 
                                in 0 < ca*(a .*. b) && 0 < ca*(b .*. c) 

main = do 
    infile <- readFile "data/triangles.txt"
    let triangles = pointsToTriangles $ intsToPoints $ streamToInts infile

    putStr "# triangles: "
    print $ length triangles

    putStr "# triangles with origin inside: "
    print $ length $ filter originInside triangles