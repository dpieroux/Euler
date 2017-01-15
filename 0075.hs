{-
Primitive triplets:
	a=m²-n², b=2mn, c=m²+n², l=a+b+c=2m²+2mn=2m(m+n)
	with m>n, m & n coprime, m-n odd

	l is even
	2l, 3l, 4l... are also valid generated

l<=L ==> 2m(m+n)<=L ==> n <= L/2m - m

2m(m+1)<=2m(m+n)<=L ==> 2m² < L

-}

import Data.Array.Unboxed

pairs :: Int -> [(Int, Int)]
pairs upBound = [(m,n) | m <- [1..m_max],
                         n <- (n_interval m),
                         gcd m n == 1]
  where
  	m_max = floor $ sqrt (fromIntegral upBound / 2)
  	n_interval m = [n0, n0+2 .. (min (m-1) (div upBound (2*m)-m))]
  	  where
  	    n0 = if (even m) then 1 else 2

perimeter m n = 2*m*(m+n)

scaledPerimeters upBound (m, n) = [p*s | s <- [1..(div upBound p)]]
  where
	p = 2*m*(m+n)

triangle (m, n) = (m*m-n*n, 2*m*n, m*m+n*n, 2*m*(m+n))

perimeters :: Int -> UArray Int Int
perimeters upBound = accumArray (+) 0 (1, div upBound 2)
	[(div p 2, 1) | p <- concat $ map (scaledPerimeters upBound) $ pairs upBound]

euler :: Int -> Int
euler = length . filter (==1) . elems . perimeters

main = print $ euler 1500000