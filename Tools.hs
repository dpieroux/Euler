module Tools( cantorPairingPairs
            , printEach
            ) where

-- | Generate the Cantor pairing pairs.
--
-- The minimum bounds tu use are provided in argument.
--
cantorPairingPairs :: Integral t => t -> t -> [(t, t)]
cantorPairingPairs b1 b2 = [(n, σ-n) | σ <- [b1+b2 .. ], n <- [b1 .. σ-b2]]


printEach :: Show a => [a] -> IO()
printEach es = printEach' 1 es
  where
    printEach' i  (e:es) = do
        putStrLn $ concat [show i, ": ", show e]
        printEach' (i+1) es

    printEach' _ [] = return ()
