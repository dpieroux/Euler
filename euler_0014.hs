import qualified Data.Map.Strict as Map

type Memoizer = Map.Map Integer Int

next n = if even n then (n `div` 2) else (3*n+1)

collatz :: Integer -> Memoizer -> (Int, Memoizer)
collatz n mem = collatz' n [] mem
  where
    collatz' :: Integer -> [Integer] -> Memoizer -> (Int, Memoizer)
    collatz' n ns mem = case Map.lookup n mem of
        Just len  -> update len ns mem
        Nothing   -> collatz' (next n) (n:ns) mem

      where
        update :: Int -> [Integer] -> Memoizer -> (Int, Memoizer)
        update len ([]) mem = (len, mem)
        update len (n:ns) mem = update (len+1) ns (Map.insert n (len+1) mem)

euler limit = euler' 2 (1, 1) (Map.singleton 1 1)
  where
    euler' n best@(best_n, best_l) mem
        | n == limit  = (best_n, best_l)
        | l' < best_l = euler' (n+1) best mem'
        | otherwise   = euler' (n+1) (n, l') mem'
      where
        (l', mem') = collatz n mem

main = print $ euler 999999