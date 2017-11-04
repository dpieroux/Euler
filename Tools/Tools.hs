module Tools(cantorPairingPairs) where

-- | Generate the Cantor pairing pairs.
--
-- The minimum bounds tu use are provided in argument.
--
cantorPairingPairs :: Integral t => t -> t -> [(t, t)]
cantorPairingPairs b1 b2 = [(n, σ-n) | σ <- [b1+b2 .. ], n <- [b1 .. σ-b2]]
