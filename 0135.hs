{-------------------------------------------------------------------------------

Given the positive integers, x, y, and z, are consecutive terms of an arithmetic
progression, the least value of the positive integer, n, for which the equation,
x² − y² − z² = n, has exactly two solutions is n = 27:

34² − 27² − 20² = 12² − 9² − 6² = 27

It turns out that n = 1155 is the least value which has exactly ten solutions.

How many values of n less than one million have exactly ten distinct solutions?

--------------------------------------------------------------------------------

Let define n(a,b) = (a+2b)² - (a+b) - a² with a and b positive.
                  = 3b² + 2ab - a²
                  = 4b² - (a-b)²

So, n(a,b) is a parabola centred at a=b where it reaches a maximum 4b². It
vanishes at a₊ = 3b and a₋ = -b. H

Because a and n are positive, the acceptable range for a is [1 3b-1].

The smallest positive value of n(a,b) is obtained for the greatest value of a
for which n is still positive, i.e. for a = 3b-1: n_min = n(3b-1, b) = 4b - 1.

As n<L with L=10⁶, it comes 4b-1<L, or b ≤ L/4.  The range of b is thus [1 L/4].

As seen above a must be in [1 3b-1] for n to be positive. However, values in the
middle of that range might be larger than L. To avoid iterating on such values,
the range is divided in two parts about a=b. The left part is iterated from a=1
to b, and the right part from a=3b-1 down to b-1. The iterations are stopped if
ever n becomes larger or equal to L.

For each pair (a,b) visited, n is computed and an array is built to count for
the occurrence of each n. The last step is then to count the number of n that
have been reached 10 times.

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
    putStrLn $ (++) "Euler 135: " (show $ euler (10^6) 10)