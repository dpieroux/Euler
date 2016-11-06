{-# LANGUAGE BangPatterns #-}

import System.CPUTime

import Data.List(sort)
import qualified Data.Map as Map

timeIt :: (a -> String) -> a -> IO ()
timeIt toString exp = do
    t1 <- getCPUTime
    let !exp' = exp
    t2 <- getCPUTime
    putStrLn $ (show $ round $ fromIntegral (t2-t1) / 10^9) ++ " \t| " ++ toString exp'

type Mem = Map.Map String [Integer]
type Continuation = (Integer, Mem, Integer)

iter :: Int -> Continuation -> ([Integer], Continuation)
iter size (bound, mem, n)
    | c < bound        = iter size (bound, mem', n+1)
    | null candidates  = iter size (10^(length k), mem'', n+1)
    | otherwise        = (best, (bound,  mem', n+1))
  where
    c = n^3
    k = sort $ show c
    values = Map.findWithDefault [] k mem
    mem' = Map.insert k (n:values) mem
    candidates = filter (\ls -> size == length ls) $ Map.elems mem
    best = head $ sort $ map reverse candidates
    mem'' = Map.fromList((k, [n]) : filter (\(k, v) -> length v < size) (Map.toList mem))

euler sizes = do euler' sizes (10, Map.empty, 1) where
    euler' (size:sizes) continuation = do
        let (res, continuation') = iter size continuation
        timeIt (\n -> (show size) ++ ": \t" ++ (show $ head n)) $ res
        euler' sizes continuation'
    euler' [] _ = return ()

main = euler [1..20]



