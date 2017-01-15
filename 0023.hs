import Data.Array 

properDividers' n q 
    | n < q²    = []
    | n == q²   = [q]
    | otherwise = if (n == q * r) then q : r : properDividers' n (q+1)
                                  else properDividers' n (q+1)
    where q² = q * q; r = quot n q

properDividers :: Int -> [Int]
properDividers n = 1 : (properDividers' n 2)

isAbundant :: Int -> Bool
isAbundant n = n < (sum . properDividers) n

limit :: Int
limit = 28123

sumOfTwoAbundants' :: [Int] -> Array Int Bool -> Array Int Bool
sumOfTwoAbundants' [] acc = acc
sumOfTwoAbundants' (y:ys) acc 
    = sumOfTwoAbundants' ys 
                         (acc // [(i, True) | i <- (filter (<=limit) (map (+y) (y:ys)))])

main = do
    print (sum [i | (i, n) <- (assocs (sumOfTwoAbundants' abundants initArray)),
                    n == False])
        where abundants = filter isAbundant [1 .. limit-12]
              initArray = listArray (1, limit) [False | i <- [1 .. limit]]
