{-------------------------------------------------------------------------------

The positive integers, x, y, and z, are consecutive terms of an arithmetic
progression. Given that n is a positive integer, the equation, x² − y² − z² = n,
has exactly one solution when n = 20:

    13² − 10² − 7² = 20

In fact there are twenty-five values of n below one hundred for which the
equation has a unique solution.

How many values of n less than fifty million have exactly one solution?

--------------------------------------------------------------------------------

This is essentially the same question as problem 135. It is solved along the
same way.

-------------------------------------------------------------------------------}

import Data.Array.Unboxed

-- Returns the all the n(a,b) smaller than the given upper bound
all_ns :: Int -> [Int]
all_ns bound = concat $ map b2ns [1.. quot bound 4]
  where
    b2ns b
        =  (takeWhile (<bound) $ map a_b2n [1 .. (b-1)])
        ++ (takeWhile (<bound) $ map a_b2n [3*b-1, 3*b-2 .. b])
      where
        a_b2n a = 4*b^2 - (a-b)^2

-- Given a bound and a list of values, returns a array mapping a value to its
-- cardinal in the list.
count :: Int -> [Int] -> UArray Int Int
count bound ns = accumArray (\n _ -> n+1) 0 (1, bound-1)
                            [(n, 1) | n <- ns]

-- Returns the number of n(a,b) < bound occurring exactly n times
euler bound n = length $ filter (== n) $ elems $ count bound $ all_ns bound

main = do
    putStrLn $ (++) "Euler 136: " (show $ euler (50*10^6) 1)