module Tools( cantorPairingPairs
            , printEach
            , revNum
            ) where

-- | Generate the Cantor pairs.
--
-- The minimum bounds to use are provided as arguments.
--
cantorPairingPairs :: Integral t => t -> t -> [(t, t)]
cantorPairingPairs b1 b2 = [(n, σ-n) | σ <- [b1+b2 .. ], n <- [b1 .. σ-b2]]

--------------------------------------------------------------------------------

printEach :: Show a => [a] -> IO()
printEach es = printEach' 1 es
  where
    printEach' i  (e:es) = do
        putStrLn $ concat [show i, ": ", show e]
        printEach' (i+1) es

    printEach' _ [] = return ()

--------------------------------------------------------------------------------

revNum :: Integral a => a -> a
revNum n = loop n 0
  where
    loop 0 acc = acc
    loop n acc = let (n', r) = quotRem n 10 in loop n' (r+10*acc)