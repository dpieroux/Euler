isqrt :: Integral a => a -> a
isqrt n = if approx^2 <= n then approx else approx-1
  where
    approx = round $ sqrt $ fromIntegral n

series n = if (isqrtn^2 == n) then [] else iter isqrtn 1
  where
    isqrtn = isqrt n
    iter a b = (a', b'): iter a' b'

      where
        b' = (n-a^2) `div` b
        d  = (isqrtn + a) `div` b'
        a' = d * b' - a
    {-
    (√n - a) / b < 1 => b / (√n - a) > 1
    b / (√n - a)
    = b (√n + a) / (n-a²)
    = (√n + a) / b'  (iif n-a² is divisible by b)
    = d + (√n - a') / b' -> d ; (a, b) -> (a', b')

    Let us show now that n-a² is divisible by b by induction.

    For the first iteration b=1, so it is demonstrated.

    Suppose now that n-a² is divisible by b. Let us show than then n-a'² is also
    divisible by b'.

    It comes:
    b' = (n-a²)/b is integer (cf induction hypothesis)

    (√n + a) = d b' + (√n - a') => a' = d b' - a

    n-a'² = n - (d b')² + 2 a d b' - a²
          = (n - a²) + b' d (2 a - b' d)
    which is divisible by b' (cf induction hypothesis for the first term)
    -}

period [] = 0
period (n:ns) = period' ns 1
  where
    period' (n':ns') res = if n==n' then res else period' ns' (res+1)

euler n = length $ filter odd $ map (period . series) [1..n]

main = do
    print $ euler 13
    print $ euler (10^4)
