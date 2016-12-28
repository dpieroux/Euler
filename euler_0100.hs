{-------------------------------------------------------------------------------

If a box contains twenty-one coloured discs, composed of fifteen blue discs and
six red discs, and two discs were taken at random, it can be seen that the
probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

The next such arrangement, for which there is exactly 50% chance of taking two
blue discs at random, is a box containing eighty-five blue discs and thirty-five
red discs.

By finding the first arrangement to contain over 10¹² = 1,000,000,000,000 discs
in total, determine the number of blue discs that the box would contain.

--------------------------------------------------------------------------------

Let b be the number of blue discs and n be the total number of disks. The
equation to solve is 
    (1)     2 b (b-1) = n (n-1) 

Solving for b gives:  
    (2)     b = {1 + sqrt[1 + 2 n (n-1)]}/2

For b to be integer, 1+2 n (n-1) must be a square:
    (3)     x² := 1 + 2 n (n-1), with x integer
    (2')    b = (1 + x)/2

Note that (3) implies that x is odd and there is no need for additional
constraint to ensure that b is integer.

Solving (3) for n gives
    (4)     n = {1 + sqrt[1 + 2 (x²-1)]}/2

As above, for n to be integer, 1 + 2 (x²-1) must be a square:
    (5)     y² := 1 + 2 (x²-1), with y integer
    (4')    n = (1 + y)/2

Note that (5) implies that y is odd and there is no need for additional
constraint to ensure that n is integer. By rewriting (5), it becomes obvious
that it is a Pell-Fermat equation:
    (5')    y² - 2x² = -1

This equation has an infinite number of solution which can be computed. We will
use here the 'Math.NumberTheory.Pell' to compute them.

It also comes
    (6)     n > 10¹² ⇒ y > 2 10¹² -1 ⇒ y ≥ 2 10¹² 

So, the approach consists in finding the first solution (x, y) of the Pell-
Fermat equation (5') such that y ≥ 2 10¹², and to compute the corresponding b
with (2').

-------------------------------------------------------------------------------}

import Math.NumberTheory.Pell as Pell

euler bound = head
            $ map (\(y, x) -> (div (1+x) 2, div (1+y) 2))
            $ filter (\(y, x) -> y > 2*bound)
            $ Pell.solve 2 (-1)
main = do
    let (b, n) = euler (10^12)
    putStr "# blue disks: "; print b
    putStr "# red disks:  "; print (n-b)
    putStr "# disks:      "; print n
    putStr "Ratio: "       ; print (fromIntegral(b*(b-1))/fromIntegral(n*(n-1)))
