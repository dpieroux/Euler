{-------------------------------------------------------------------------------

Investigating progressive numbers, n, which are also square

Problem 141

A positive integer, n, is divided by d and the quotient and remainder are q and
r respectively. In addition d, q, and r are consecutive positive integer terms
in a geometric sequence, but not necessarily in that order.

For example, 58 divided by 6 has quotient 9 and remainder 4. It can also be seen
that 4, 6, 9 are consecutive terms in a geometric sequence (common ratio 3/2).
We will call such numbers, n, progressive.

Some progressive numbers, such as 9 and 10404 = 102², happen to also be perfect
squares. The sum of all progressive perfect squares below one hundred thousand
is 124657.

Find the sum of all progressive perfect squares below one trillion (10^12).

--------------------------------------------------------------------------------

So, we have to solve n² = qd+r with q, d and r forming a geometric sequence.

By construction r < d; therefore they are four cases to consider:
    1. q < r < d
    2. r < q < d
    3. r < d < q

Cases and 3 are really equivalent, since what's important is the product qd.
So we can drop the third case without loss of generality.

Case 1: q < r < d
-----------------

If q < r < d, then qd = r² and n² = r² + r
    => 4n² = 4r² + 4r = (2r + 1)² - 1
    => (2r + 1)² - (2n)² = 1

Since there are no square numbers whose difference is 1, this equation has no
solution


Case 2: r < q < d
-----------------

Because r, q and d form a geometric sequence, it comes that r = su², q = suv,
d=sv² with 1≤u<v and 1≤s.

=>  qd + r = s²uv³ + su² < b (b being the bound, i.e. 10^12 in the exercise)

    u≥1, s≥1 => v³ ≤ s²uv³ + su² < b => v³ < b

    u≥1      => s²v³ < s²v³ + s < s²uv³ + su² < b => s² < b/v³

=> we choose v such that 2 ≤ v, v³ < b
             s           1 ≤ s, s² < b/v³
             u           1 ≤ u < v, su (sv³ + u) < b

Note that the values generated are not all different. Therefore it is necessary to remove the duplicates.

-------------------------------------------------------------------------------}

import qualified Data.Set as Set
import Math.NumberTheory.Powers.Squares(isSquare')

-- Return all values of qd+r, with q, d and r forming a geometric sequences, up
-- to the given bound.

genValues b = Set.fromList
            $ filter isSquare'
            $ [s*u*(sv3+u)
                  | v <- takeWhile (\v -> v^3 < b) [2..]
                  , let v3 = v^3
                  , s <- takeWhile (\s -> v3 * s^2 < b) [1..]
                  , let sv3 = s*v3
                  , u <- takeWhile (\u -> s*u*(sv3+u) < b) [1..v-1]
              ]

euler b = Set.foldl' (+) 0 $ genValues b

main = putStrLn $ concat [ "Euler 141, 10^5: ",  show $ euler (10^5), "\n"
                         , "Euler 141, 10^12: ", show $ euler (10^12)]