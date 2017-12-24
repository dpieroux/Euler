{-------------------------------------------------------------------------------

Searching for a maximum-sum subsequence
Problem 149

Looking at the table below, it is easy to verify that the maximum possible sum
of adjacent numbers in any direction (horizontal, vertical, diagonal or anti-
diagonal) is 16 (= 8 + 7 + 1).

       -2   5   3   2
        9  -6   5   1
        3   2   7   3
       -1   8  -4   8

Now, let us repeat the search, but on a much larger scale:

First, generate four million pseudo-random numbers using a specific form of what
is known as a "Lagged Fibonacci Generator":

For 1 ≤ k ≤ 55, sk = [100003 - 200003k + 300007k³] (modulo 1000000) - 500000.
For 56 ≤ k ≤ 4000000, sk = [sk-24 + sk-55 + 1000000] (modulo 1000000) - 500000.

Thus, s10 = -393027 and s100 = 86613.

The terms of s are then arranged in a 2000×2000 table, using the first 2000
numbers to fill the first row (sequentially), the next 2000 numbers to fill the
second row, and so on.

Finally, find the greatest sum of (any number of) adjacent entries in any
direction (horizontal, vertical, diagonal or anti-diagonal).

--------------------------------------------------------------------------------

This problem can be reduced to finding the greatest sum of a set of list.

-------------------------------------------------------------------------------}

import Data.Array.Unboxed

laggedFibonacciGen :: () -> [Int]
laggedFibonacciGen () = seq
  where
    a = [f (100003 - 200003*k + 300007*k^3) | k <- [1..55]]
    seq = (++) a $ map f $ zipWith (\x y -> x + y) (drop 31 seq) seq
    f n = mod n 1000000 - 500000

randomNums = laggedFibonacciGen ()

sample = [ -2,  5,  3,  2
         ,  9, -6,  5,  1
         ,  3,  2,  7,  3
         , -1,  8, -4,  8]

greatestSum es = iter es 0 0
  where
    iter (e:es) cur best = let cur' = cur + e
                           in if (cur' <= 0) then iter es 0    best
                                             else iter es cur' (max cur' best)
    iter [] _   max = max

segments :: [Int] -> Int -> [[Int]]
segments es dim = concat [rows, cols, diags, aDiags]
  where
    b = dim - 1 -- bound
    table :: UArray (Int, Int) Int
    table = listArray ((0,0), (b,b)) $ es

    rows   = [[table ! (i, j) | j  <- [0 .. b]] | i <- [0..b]]
    cols   = [[table ! (i, j) | i  <- [0 .. b]] | j <- [0..b]]
    diags  = [[table ! (i0+k, k)   | k  <- [(max 0 (-i0)) .. min b (b-i0)]]
                                   | i0 <- [-b..b]]
    aDiags = [[table ! (i0+k, b-k) | k  <- [(max 0 (-i0)) .. min b (b-i0)]]
                                   | i0 <- [-b..b]]

euler :: [Int] -> Int -> Int
euler es dim = maximum $ map greatestSum $ segments es dim

main = putStrLn $ concat
     $ [ "Euler 149:"
       , "\n    s10  = ", show $ randomNums !! 9
       , "\n    s100 = ", show $ randomNums !! 99
       , "\n    sample solution = ", show $ euler sample 4
       , "\n    acutal solution = ", show $ euler randomNums 2000
       , "\n"]