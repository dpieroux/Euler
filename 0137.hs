{-------------------------------------------------------------------------------

Consider the infinite polynomial series AF(x) = xF1 + x²F₂ + x³F₃ + ..., where
F_k is the kth term in the Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ; that is,
F_k = F_k−1 + F_k−2, F_1 = 1 and F_2 = 1.

For this problem we shall be interested in values of x for which AF(x) is a
positive integer.

Surprisingly AF(1/2) = (1/2).1 + (1/2)².1 + (1/2)³.2 + (1/2)⁴.3 + (1/2)⁵.5 + ...
                     = 1/2 + 1/4 + 2/8 + 3/16 + 5/32 + ...
                     = 2

The corresponding values of x for the first five natural numbers are shown
below.
                x     AF(x)
            √2−1        1
            1/2         2
            (√13−2)/3   3
            (√89−5)/8   4
            (√34−3)/5   5

We shall call AF(x) a golden nugget if x is rational, because they become
increasingly rarer; for example, the 10th golden nugget is 74049690.

Find the 15th golden nugget.

--------------------------------------------------------------------------------

AF(x) is the generating function of the Fibonacci sequence. Using the Fibonacci
recurrence relation it is trivial to show that AF(x) = x/(1-x-x²).

Posing AF(x) = n and solving for x, it comes
    x = [√(5n²+2n+1) - (n+1)]/2n

The other root is negative, so it is discarded.

x is rational iif 5n²+2n+1 is a perfect square.

--------------------------------------------------------------------------------

This approach is correct but much too slow.

-------------------------------------------------------------------------------}

import Math.NumberTheory.Powers.Squares
import Tools

goldenNuggets :: [Integer]
goldenNuggets = [n | n <- [1..], isSquare (5*n^2+2*n+1)]

main = do
    printEach $take 15 goldenNuggets
