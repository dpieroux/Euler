import Debug.Trace(trace)

iter b e res
    | b^e < inf_bound = iter (b+1) e res
    | b^e < sup_bound = iter b (e+1) (res + 10 - b)
    | otherwise       = res
  where
    inf_bound = 10^(e-1)
    sup_bound = 10^e

euler = iter 1 1 0

main = print $ euler