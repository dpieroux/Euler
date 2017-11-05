{-------------------------------------------------------------------------------

Consider the isosceles triangle with base length, b = 16, and legs, L = 17.

By using the Pythagorean theorem it can be seen that the height of the triangle,
h = √(17^2 − 8^2) = 15, which is one less than the base length.

With b = 272 and L = 305, we get h = 273, which is one more than the base
length, and this is the second smallest isosceles triangle with the property
that h = b ± 1.

Find ∑ L for the twelve smallest isosceles triangles for which h = b ± 1 and b,
L are positive integers.

--------------------------------------------------------------------------------

It comes l² = (b/2)^2 + h^2 and h = b ± 1

Because h and l are integer, b must be even. Let pose b=2β.

Then:   l² = β² + h² and h = 2β ± 1.
           = 5β² ± 4β + 1

Let's multiply by 5 each sides:
        5l² = 25β² ± 20β + 5 = (5β ± 2)² + 1
        => (5β ± 2)² - 5l² = -1

This is a generalised Pell's equation.

Note: given (x, y) solution of x² - 5 y² = -1, β is integer if x quot 5 is 2 or
3.
-------------------------------------------------------------------------------}

import qualified Math.NumberTheory.Pell as Pell

euler = sum
      $ take 12
      $ map snd
      $ tail -- The first solution matches the degenerated case b=0 and h=1
      $ filter (\(x, y) -> (\n -> n == 2 || n == 3) $ mod x 5)
      $ Pell.solve 5 (-1)

main = do
    putStrLn $ concat ["Euler 128: ", show euler]
