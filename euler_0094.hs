{-
It is not too difficult to show that an almost equilateral triangle must have
a base of length 2a, a height of length b and equal sides of length 2a+-1, with
(a, b, 2a+-1) forming a primitive Pythagorean triple.

Starting from the tree of primitive Pythagorean triples (see Wiki), it can then
be demonstrated that all triples (a, b, 2a+-1) are generated from the triple
(3, 4, 5) by successive application of the transformation
(x, y, z)->(-2*x+y+2*z, -x+2*y+2*z, -2*x+2*y+3*z).
-}

nextTriangle :: (Int, Int, Int) -> (Int, Int, Int)
nextTriangle (x, y, z) = (-2*x+y+2*z, -x+2*y+2*z,-2*x+2*y+3*z)

main = do
    putStrLn $ show
        $ sum $ takeWhile (<=10^9)
        $ map (\(a, h, c) -> 2*(a+c))
        $ iterate nextTriangle (3, 4, 5)