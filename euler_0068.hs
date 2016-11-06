{-

Let write the magic 5-gon ring as follows:

   a
     \
       a'    b
     /   \  /           a  b  c  d  e
   e'      b'        => a' b' c' d' e' => a b c d e / a' b' c' d' e'
  / \     /
 e   d'--c'--c
      \
       d

The representation proposed by the Euler site is then given by:
    a a' b b' c c' d d' e e'

1.  The nodes are linked by the following relations:
        a+a'+b' = lambda
        b+b'+c' = lambda
        c+c'+d' = lambda
        d+d'+e' = lambda
        e+e'+a' = lambda
    => 5 relations, but one additional free parameter (lambda)

    This allows us to define an algebraic basis of
        10 (nodes) + 1 (free parameter) - 5 (relations) = 6 elements

        m1 = 10000/00101
        m2 = 01000/10010
        m3 = 00100/01001
        m4 = 00010/10100
        m5 = 00001/01010
        m6 = 00000/11111

    Any "magic" ring can thus be written as S = l1 m1 + l2 m2 +... l6 m6.

2.  It comes that a=l1, .. , e=l5. So, l1 .. l5 must all be different and
    comprised in between 1 and 10.

4. To ensure that A is the smallest external node, l1 must be smaller than
   l2, .., l5. This also restraints the value of l1 from 1 to 6 included.

5. All the numbers from 1 to 10 being present exactly once, it comes that the
   sum of the node must be equal to 1 + .. + 10 = 55

    Using the mode decomposition, we find S = 3*(l1 + .. + l5) + 5 l6. As S must
    be 55 and that l6 is multiplied by 5, this implies that
        l1 + .. + l5 must be a multiple of 5 and
        l6 = (55 - 3*(l1 + .. + l5))/5
-}

import Data.List(nub, permutations)

-- Return the arrangements of n elements of the list ls
arrangements :: Int -> [a] -> [[a]]
arrangements n ls'@(l:ls)
	| n > 1     = map (l:) (arrangements (n-1) ls) ++ (arrangements n ls)
	| n == 1    = [[l] | l <- ls']
	| otherwise = []
arrangements _ [] = []

nodes = filter (\ls -> 10 == length ls)
	  $ map nodes
  	  $ concat
  	  $ map (\(l:ls) -> map (l:) $ permutations ls)
	  $ filter (\mode -> sum mode `mod` 5 == 0)
	  $ takeWhile (\(l:ls) -> (l<=6))
  	  $ arrangements 5 [1..10]
  where
  	nodes mode@[a, b, c, d, e]
  		= filter (\n -> 1<=n && n<=10)
  		$ nub [a, a', b, b', c, c', d, d', e, e']
  	  where
 		l6 = 11 - 3 * (sum mode `div` 5)
 		a' = b+d+l6
 		b' = c+e+l6
 		c' = d+a+l6
 		d' = e+b+l6
 		e' = a+c+l6


euler = map (concat . map show) $ map ring nodes
  where
  	ring [a, a', b, b', c, c', d, d', e, e']
 		= [a, a', b', b, b', c', c, c', d', d, d', e', e, e', a']

main = print $ maximum $ filter (\result -> length result == 16) euler