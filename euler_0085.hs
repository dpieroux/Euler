{-
In a rectangle of m by n, the number of sub-rectangles is given by
    N(m,n) = m(m+1)/2 x n(n+1)/2.

We are looking for the pair m and n such R(m,n) is the closest from N = 2e6.

Because of the symmetry, we can restrain our search to m <= n and thus m varies
from 1 up to the root of m (m+1) = 2N. then, for a given m, then we have to find
the value of n such that N(m, n) is closest to N*.

The value of n researched is the integer below or above the root of the equation
    n² + n = 4 N/m/(m+1).
   
-}

root_quadratic_eq :: Double -> Double -> Double -> Double
root_quadratic_eq a b c = ((-b) + sqrt (b*b-4*a*c))/2/a

euler :: Int -> (Int, Int)
euler target = euler' 1 1 0 target 
  where
    euler' :: Int -> Int -> Int -> Int -> (Int, Int)
    euler' m best_m best_n best_delta 
        | n<m                = (best_m, best_n)
        | delta < best_delta = euler' (m+1) m n delta
        | otherwise          = euler' (m+1) best_m best_n best_delta 
      where
        (n, delta) = get_best_n m
    
    target' :: Double
    target' = fromIntegral target

    get_best_n :: Int -> (Int, Int)
    get_best_n m  = if (delta1<delta2) then (n, delta1) else (n+1, delta2)
      where
        n = floor $ root_quadratic_eq 1.0 1.0 ((-4.0)*target'/m'/(m'+1))
        
        m' = fromIntegral m
        prod0 = m*(m+1)*(n+1)
        prod1 = prod0 * n      -- N(m, n)
        prod2 = prod0 * (n+2)  -- N(m, n+1)
        delta1 = 4*target - prod1
        delta2 = prod2 - 4*target

main = do 
    putStrLn $ show (best_m, best_n) ++ " => " ++ show (best_m * best_n)
  where
    (best_m, best_n) = euler 2000000