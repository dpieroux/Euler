{-------------------------------------------------------------------------------

Perfect Square Collection
Problem 142

Find the smallest x + y + z with integers x > y > z > 0 such that x + y, x − y,
x + z, x − z, y + z, y − z are all perfect squares.

--------------------------------------------------------------------------------

Let's pose 
    x+y = A², y+z = B², x+z = C²
    x-y = a², y-z = b², x-z = c²

Then it comes:
    n = x+y+z = (A² + B² + C²)/2 => A² + B² + C² must be even.
    x = n-B², y = n-C², z = n-A²
    
    A > C > B > b; a < c > b; A > a, B > b, C > c.

To solve the problem, the key relations to consider are A²=B²+c² and A²=C²+b²
because they correspond to two Pythagorean triples with a same hypotenuse A.
Since C is bigger than B, b and c, these two triplets must also be different.

So, the principle of the algorithm is to generate all the triplets, and to
consider all pairs of triplets sharing a same hypotenuse, to deduce the
corresponding A, B, C, b and c, and to check that all conditions are fulfilled.
In fact, starting from two triplets with a same hypotenuse, the only extra
condition to check is that x-y = c² - b² is a square.

Now suppose that we have found a first pair of triplets that fulfil all the
conditions. It is not certain that it is the one with the smallest n. So, we
generate all triplets whose hypotenuse squared is not bigger than 3 times the
one of the candidate. Because the minimum n must be smaller then (A²+B²+C²)/2 <
3A²/2 of the first pair of triplets, we'll then be certain to have all the
triplets amongst which the pair with minimal n will be found. So, we again look
for all pairs of triplets sharing a same hypotenuse and fulfilling the
conditions, to return the one with the smallest n.

-------------------------------------------------------------------------------}

import Data.List
import Tools.Pythagoras
import Math.NumberTheory.Powers.Squares(isSquare')

-- getValidTripletPairs' takes a list of Pythagorean triples sharing a same
-- hypotenuse, find matching pairs, and return the corresponding x, y, z and n.
getValidTripletPairs' :: [(Int, Int, Int)] -> [(Int, Int, Int, Int)]
getValidTripletPairs' es = concat $ map (validTripletPairs' (head es)) (tail es)
  where
    validTripletPairs' a (p, q, r) = concat [check' a (p, q, r), 
                                             check' a (q, p, r)]
      where 
        check' (vb, vC, vA) (vc, vB, _) 
          = if isSquare' (vc*vc-vb*vb) && r == 0 then [(x, y, z, n)] else [] 
          where
            vA2 = vA*vA
            vB2 = vB*vB
            vC2 = vC*vC
            (n, r) = quotRem (vA2 + vB2 + vC2) 2
            x = n - vB2
            y = n - vC2
            z = n - vA2 

-- getValidTripletPairs takes a list of Pythagorean triples, find matching
-- pairs, and return the corresponding x, y, z and n.
getCandidates 
     = concat . map getValidTripletPairs' 
     . concat . map ((filter notSingleton) . tails) 
     . filter notSingleton 
     . groupBy (\(_, _, a) (_, _, b) -> a == b)
  where
    notSingleton (_:_:_) = True
    notSingleton _ = False

    -- getValidTripletPairs' takes a list of Pythagorean triples sharing a same
    -- hypotenuse, find matching pairs, and return the corresponding x, y, z and
    -- n.
    getValidTripletPairs' :: [(Int, Int, Int)] -> [(Int, Int, Int, Int)]
    getValidTripletPairs' es = concat $ map (validTripletPairs' (head es)) (tail es)
      where
        validTripletPairs' a (p, q, r) = concat [check' a (p, q, r), 
                                                 check' a (q, p, r)]
          where 
            check' (vb, vC, vA) (vc, vB, _) 
              = if isSquare' (vc*vc-vb*vb) && r == 0 then [(x, y, z, n)] else [] 
              where
                vA2 = vA*vA
                vB2 = vB*vB
                vC2 = vC*vC
                (n, r) = quotRem (vA2 + vB2 + vC2) 2
                x = n - vB2
                y = n - vC2
                z = n - vA2 


euler = head 
      $ sortBy (\(_, _, _, n1) (_, _, _, n2) -> compare n1 n2) solutions
  where
    bound = let (x, y, _, _) = head $ getCandidates $ genTriples () 
            in  3 * (x + y)
    solutions = getCandidates 
              $ takeWhile (\(_,_,h) -> h*h < bound) $ genTriples ()

main = putStrLn $ concat $ ["Euler 142: (x, y, z, n) = ", 
                            show $ euler] 
