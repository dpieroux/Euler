{-------------------------------------------------------------------------------

Let (a, b, c) represent the three sides of a right angle triangle with integral
length sides. It is possible to place four such triangles together to form a
square with length c.

For example, (3, 4, 5) triangles can be placed together to form a 5 by 5 square
with a 1 by 1 hole in the middle and it can be seen that the 5 by 5 square can
be tiled with twenty-five 1 by 1 squares.

However, if (5, 12, 13) triangles were used then the hole would measure 7 by 7
and these could not be used to tile the 13 by 13 square.

Given that the perimeter of the right triangle is less than one-hundred million,
how many Pythagorean triangles would allow such a tiling to take place?

--------------------------------------------------------------------------------

Notation and definition:
    - Given a triangle T, let's (s, m, h) be the associated Pythagorean triple
      with s < m and s²+m²=h², i.e s is the length of the small, medium and hypotenuse side respectively.
    - A triangle is said to be 'valid' if it fulfils the condition.
    - By extension, a Pythagorean triple (s, m, h) is valid if the corresponding
      triangle is valid

Given T(s, m, h), the length of the internal hole square side is m-s. Such a square allows covering the external square iif m-s | h.

Note that it is enough to consider the primitive triples since, if a triple is
valid, then km-ks | kh => m-s | h and thus the corresponding primitive is also
valid.

Let a = p²-q², b = 2pq and c = p²+q²; (s,m) = (a,b) if a<b or (b,a) if b<a.

The perimeter P = a + b + c = 2 p² + 2 pq = 2p(p+q) ≤ 2*p(p-1)

-------------------------------------------------------------------------------}

import Data.List

euler limit = foldl' (\acc p -> acc + quot limit p) 0 perimeters
  where
    -- Perimeters is the list of perimeters from primitive triangles that fulfil
    -- the criterion.
    perimeters = [ perim
                 | p <- takeWhile (\p -> 2*p*(p-1) < limit) [1 ..]
                 , q <- [p-1, p-3 .. 1]
                 , 1 == gcd p q
                 , let p² = p*p
                 , let q² = q*q
                 , let a = p² - q²
                 , let b = 2*p*q
                 , let h = p² + q²
                 , rem h (abs (b-a)) == 0
                 , let perim = a + b + h
                 , perim < limit
                 ]

main = do
    putStrLn $ concat $ ["Euler 139: ", show $ euler (10^8)]

