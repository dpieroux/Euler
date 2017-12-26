{-------------------------------------------------------------------------------

In a triangular array of positive and negative integers, we wish to find a sub-
triangle such that the sum of the numbers it contains is the smallest possible.

In the example below, it can be easily verified that the marked triangle
satisfies this condition having a sum of -42.

     15
    -14 >  -7 <
     20 > -13  -5 <
     -3 >   8  23  -26 <
      1 >  -4  -5  -18   5 <
    -16    31   2    9  28  3

We wish to make such a triangular array with one thousand rows, so we generate
500500 pseudo-random numbers sk in the range ±219, using a type of random number
generator (known as a Linear Congruential Generator) as follows:

t := 0
for k = 1 up to k = 500500:
    t := (615949*t + 797807) modulo 2^20
    sk := t-2^19

Thus: s1 = 273519, s2 = -153582, s3 = 450905 etc

Our triangular array is then formed using the pseudo-random numbers thus:
    s1
    s2  s3
    s4  s5  s6
    s7  s8  s9  s10
    ...

Sub-triangles can start at any element of the array and extend down as far as we
like (taking-in the two elements directly below it from the next row, the three
elements directly below from the row after that, and so on).

The "sum of a sub-triangle" is defined as the sum of all the elements it
contains. Find the smallest possible sub-triangle sum.

--------------------------------------------------------------------------------

Let e(i, j) be the element located at row i and column j, and S(k, i, j) be the
sum of the elements of the triangle whose top is located at (i, j) and whose
length is k. The length is defined such that a triangle of length k has k*1
numbers on each of its sides. Therefore a triangle of length 0 resumes to a
single point.

Then it comes:

    S(0, i, j) = e(i, j)
    S(1, j, 1) = e(i, j) + e(i+1, j) + e(i+1, j+1)
    S(k, i, j) = e(i, j) + S(k-1, i+1, j) + S(k-1, i+1, j+1) - S(k-2, i+2, j+1)

The problem can be solved recursively by computing all the S(k, i, j).

A triangle of dimension 1000 has 1000(1000+1)/2 = 500500 elements.

-------------------------------------------------------------------------------}

import Data.List

-- Random number generator
randomGen :: () -> [Int]
randomGen () = map (\n -> let r = n-c1 in seq r r)
             $ tail
             $ iterate (\t -> let r = mod (615949*t + 797807) c0 in seq r r) 0
  where
    c0 = 2^20
    c1 = 2^19

-- Resolve the problem
euler :: Int -> [Int] -> Int
euler dim es = iter (tail t) (repeat (repeat 0)) (min' t)
  where
    t = take dim $ triangulise es

    iter :: [[Int]] -> [[Int]] -> Int -> Int
    iter t1 t2 best
      = let a = map tail t1
            b = map tail t2
            c = zipWith4 (zipWith4 (\p q r s -> p+q+r-s)) t t1 a b
        in
            if null c then best
                      else iter (tail c) (tail t1) $ min best $ min' c

    min' :: [[Int]] -> Int
    min' = minimum . map minimum

    triangulise :: [Int] -> [[Int]]
    triangulise es = iter 1 es
      where
        iter i es = let (row, es') = splitAt i es in row : iter (i+1) es'


sample = [ 15,
          -14,  -7,
           20, -13, -5,
           -3,   8, 23, -26,
            1,  -4, -5, -18,  5,
          -16,  31,  2,   9, 28,  3]

main = putStrLn $ concat
    [ "Euler 150"
    , "\n    Sample: ", show $ euler 6 sample
    , "\n    Actual: ", show $ euler 1000 $ randomGen ()
    , "\n"]