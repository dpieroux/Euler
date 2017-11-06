{-------------------------------------------------------------------------------

Modified Fibonacci golden nuggets
*********************************

Consider the infinite polynomial series AG(x) = xG₁ + x²G₂ + x³G₃ + ..., where
G_k is the kth term of the second order recurrence relation G_k = G_(k−1) + G(k−2), G₁ = 1 and G₂ = 4; that is, 1, 4, 5, 9, 14, 23, ... .

For this problem we shall be interested in values of x for which AG(x) is a
positive integer.

The corresponding values of x for the first five natural numbers are shown
below.
                x       AG(x)
            (√5−1)/4      1
            2/5           2
            (√22−2)/6     3
            (√137−5)/14   4
            1/2           5

We shall call AG(x) a golden nugget if x is rational, because they become
increasingly rarer; for example, the 20th golden nugget is 211345365.

Find the sum of the first thirty golden nuggets.

--------------------------------------------------------------------------------

We follow the same approach as problem 137:


                                  AG(x) = x G₁ + x²G₂ + x³G₃ + ...,
                                x Ag(x) =        x²G₁ + x³G₂ + ...,
                            (1+x) AG(x) = x G₁ + x²G₃ + x³G₄ + ...,
      x G₁ + x²G₂ + x(1+x) AG(x) - x²G₁ = x G₁ + x²G₂ + x³G₃ + ...,
      x    + 4 x² + x(1+x) Ag(x) - x²   = Ag(x)

    => Ag(x) = x(1+3x)/(1-x-x²)

Posing Ag(x) = q and solving for x gives

        √(5n²+14n+1) - (n+1)
    q = --------------------
              2(n+3)

q is rational if 5n²+14n+1 is a perfect square:

        l² = 5 n² + 14n + 1
       5l² = 25n² + 70n + 5 = (5n)² + 14 (5n) + 5
                            = (5n + 7)² - 44

        => (5n + 7)² - 5l²= 44

We thus obtained a generalised Pell equation in (5n+7). Solving it returns the
set of values n for which Ag(n) is rational.

-------------------------------------------------------------------------------}

import qualified Math.NumberTheory.Pell as Pell

euler = tail -- the first solution is n=0, which is invalid
      $ map (\fnps -> quot (fnps - 7) 5) -- fnps = five n plus seven
      $ filter (\fnps -> mod fnps 5 == 2)
      $ map fst $ Pell.solve 5 44

line n = concat [show n, "th golden nugget: ", show $ euler !! (n-1)]


main = do
    putStrLn $ line 20
    putStrLn $ concat $ ["Euler 140: ", show $ sum $ take 30 $ euler ]
