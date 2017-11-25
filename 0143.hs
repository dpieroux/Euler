{-------------------------------------------------------------------------------

Investigating the Torricelli point of a triangle Problem 143

Let ABC be a triangle with all interior angles being less than 120 degrees. Let
X be any point inside the triangle and let XA = p, XC = q, and XB = r.

Fermat challenged Torricelli to find the position of X such that p + q + r was
minimised.

Torricelli was able to prove that if equilateral triangles AOB, BNC and AMC are
constructed on each side of triangle ABC, the circumscribed circles of AOB, BNC,
and AMC will intersect at a single point, T, inside the triangle. Moreover he
proved that T, called the Torricelli/Fermat point, minimises p + q + r. Even
more remarkable, it can be shown that when the sum is minimised, AN = BM = CO =
p + q + r and that AN, BM and CO also intersect at T.

If the sum is minimised and a, b, c, p, q and r are all positive integers we
shall call triangle ABC a Torricelli triangle. For example, a = 399, b = 455, c
= 511 is an example of a Torricelli triangle, with p + q + r = 784.

Find the sum of all distinct values of p + q + r ≤ 120000 for Torricelli
triangles.

--------------------------------------------------------------------------------

The solution to the problem is based on the fact that the three angles between
AT, BT and CT equal 2π/3 [1]. This implies that

    c² = p² + pq + q²
    b² = p² + pr + r²
    a² = q² + qr + r²

Let us introduce the notation (a, b) to indicate that a² + ab + b² is a perfect
square. So we need to find p, q and r such that (p, q), (q, r) and (p, r).

p, q and r must all be different. Indeed, if for instance p=q, then c² = 3p² and
c = √3 p can't be integral. We then impose p>q>r to avoid similar triangles.

The algorithm starts by constructing a table that contains, for each potential
value of p = 3 .. N-3 with N the bound (i.e. 120,000 in the exercise), all the
value q such that q<p and (p, q).

Then, we consider all p+s+r
    for all p in [3 .. N-3],
    |   for all q such that (p, q) belongs to the table,
    |   |   for all r such that
    |   |   |   (q, r) and (p, r) belongs to the table
    |   |   |   p+q+r ≤ N

extracted from the table. Using again the table, it is checked that (p, r). It
is still necessary to check that p+q+r ≤ N to retain a triplet (p, q, r)

Finally duplicates values are removed by putting all the values in a set (via
Set.fromList).

[1] https://en.wikipedia.org/wiki/Fermat_point

-------------------------------------------------------------------------------}

import Data.Array.Unboxed
import Data.Set (fromList)
import Math.NumberTheory.Powers.Squares (isSquare')

-- Answers the problem for a given bound n.
euler :: Int -> Int
euler n
  = sum $ fromList [p+q+r | p <- [3 .. n-3]
                          , q <- table ! p
                          , r <- table ! q
                          , isElemOf r (table ! p)
                          , p+q+r <= n]
  where
    -- table associates p to the list of q such that p² + pq + q² is a perfect
    -- square
    table :: Array Int [Int]
    table = listArray (1, n) [qs p | p <- [1 .. n-3]]
      where
        qs p = [q | q <- [1 .. min (p-1) (n-p-1)]
                  , isSquare' (p*p+p*q+q*q)]

    -- isElemOf checks of an element is part of a list, by taking advantage that
    -- the list is ordered by increasing values.
    isElemOf e (l:ls)
      | l < e = isElemOf e ls
      | e < l = False
      | otherwise = True
    isElemOf _ _ = False


main = putStrLn $ concat $ ["Euler 143 = ", show $ euler 120000]
