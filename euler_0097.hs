
value :: Int -> Int
value n = value' n 28434
  where
    base = 10^10 :: Int   
    value' :: Int -> Int -> Int
    value' 0 acc = acc
    value' n acc = seq acc' (value' (n-1) acc')
      where 
        acc' = rem (acc*2-1) base

main = print $ value 7830457