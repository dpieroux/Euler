next row = 1 : zipWith (+) (next row) (tail row)
matrix = iterate next $ repeat 1


main = do
    print $ matrix !! 2 !! 2
    print $ matrix !! 20 !! 20