{- 

Let a, b and c be the length of the edges of the cuboide along the axis X, Y and
Z with a ≥ b ≥ c ≥ 1.

Suppose that the spider moves in straight line on the face XY and joins the edge
of length a opposite to its starting point at the location (λ a, b, 0) and
then continue its move up to its destination point (a, b, c).

This distance it travels is given by:
    
    l(λ) = (λ²a² + b²)^½ + ((1-λ)²a² + c²)^½. 

To find the value of λ that minimizes that distance, we solve ∂l(λ)/∂λ=0 and get
λ=b/(b+c). This leads in turn, after simplification to:

    l_min = (a² + (b+c)²)^½

The other shortest path candidates are obtained by permuting a, b and c.
However, from the three possibilities, the expression above corresponds to the
shortest path as a is not smaller than b and c.

Let pose d=b+c. It comes that 2 ≤ d ≤ 2a. Given a, we have to find the values of
d such that a²+d² is a perfect square. 

For each of them, the valid values of b range from b_min = floor((d-1)/2)+1 to
b_max = min(a, d-1). The length of this interval is thus
    b_max - b_min + 1 =  min(a, d-1) - floor((d-1)/2)
-}

isPerfectSquare :: Int -> Bool
isPerfectSquare n2 = (n*n == n2) where n = round $ sqrt $ fromIntegral n2

candidates :: [(Int, Int)]
candidates = [(a, d) | a <- [1..], d <- [2..2*a], isPerfectSquare (a*a+d*d)]

accumulate acc ((a, d):ls) = (a, acc') : accumulate acc' ls 
    where acc' = acc + (min a (d-1)) - (div (d-1) 2)

euler n =  head $ dropWhile (\(a, acc) -> acc<n) $ accumulate 0 candidates

main = print $ euler 1000000
