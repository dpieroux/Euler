{-------------------------------------------------------------------------------

In the following equation x, y, and n are positive integers.
 
        1/x + 1/y = 1/n

It can be verified that when n = 1260 there are 113 distinct solutions and this
is the least value of n for which the total number of distinct solutions exceeds
one hundred.

What is the least value of n for which the number of distinct solutions exceeds
four million?

--------------------------------------------------------------------------------

The solution proposed in 0110.hs works and allows to find the right answer
quickly, however, demonstrating that it is the best answer is very long, because
it is necessary to go up to Sum(ei) = 53.

The problem in that approach is that it is evaluating sequences of exponents
that, from the previous iterations, are obviously not solutions. Suppose indeed
that the sequence [ei] leads to enough distinct solutions, but that the
corresponding value n is greater than the current solution. There is no need to
check for any sequence [e'i] such that e'i <= e'i: it will of course leads to
enough distinct solutions too, but the corresponding n' will even be greater.

Therefore the idea is to build a new set of sequences [e'i] from a previous set
of sequence [ei], from which solutions leading to no improvement have been
removed.

-------------------------------------------------------------------------------}

import Data.Maybe
import Data.List
import Data.List.Unique

import Math.NumberTheory.Primes.Sieve

import Debug.Trace

-- ExpoSeq is a sequence f prime exponents
type ExpoSeq = [Int]

-- Generate all the (non-increasing) exponent sequences, for a given sum.
genExpoSeq :: Int -> [ExpoSeq]
genExpoSeq 0 = [[]]
genExpoSeq sum = concat [ map (e:) ess | e <- [1 .. sum]
                        , let ess = genExpoSeq (sum-e) 
                        ]


-- Given a set ess of sequences es_i of same sum S(es_i), generate a new set
-- ess' of sequences es'_j such that, S(es') = S(es)+1. Note that it is
-- important to remove duplicated elements to avoid a combinatory explosion.

iterExpoSeqs :: [ExpoSeq] -> [ExpoSeq]
iterExpoSeqs = sortUniq . concat . map iterExpoSeq



-- Same as iterExpoSeqs, but for a single sequence.

iterExpoSeq :: ExpoSeq -> [ExpoSeq]
iterExpoSeq = filter nonIncreasing . iterExpoSeq'
  where 
    nonIncreasing (e1:es@(e2:_)) = e1 >= e2 && nonIncreasing es
    nonIncreasing [e] = True
    
    iterExpoSeq' :: ExpoSeq -> [ExpoSeq]
    iterExpoSeq' [] = [[1]]
    iterExpoSeq' (e:es) = ((e+1):es) : (map (e:) (iterExpoSeq es))
    


-- Return the number of distinct solutions for a number n given by its sequence
-- of prime exponents.

nbrSolutions :: ExpoSeq -> Int
nbrSolutions = (`div` 2) . (+1) . product . map (\e -> 2*e+1)

aboveAndBelowBound :: Int -> [ExpoSeq] -> ([ExpoSeq], [ExpoSeq])
aboveAndBelowBound bound = partition (\es -> bound < nbrSolutions es)



-- Given the exponent sum S, the largest number of distinct solutions is
-- obtained by the sequence e0 = ... = eS = 1, which displays (3^S + 1)/2
-- solutions. This gives a lower bound for S below which there is no acceptable
-- solution:
-- *        bound < (3^S + 1)/2  ==>  2*bound <= 3^S

lowerExponentsSum :: Int -> Int
lowerExponentsSum bound = fst . head 
             $ filter ((2*bound <=) . snd) 
             $ zip [0..] (iterate (*3) 1)

upperBoundForN :: Int -> Integer
upperBoundForN expoSum = exponentsToNumber $ take expoSum $ repeat 1



-- Main routine.
--
-- Its starts with the exponent sequences having the smallest sum but displaying
-- at least a possible solution. Then the sequences are iterated. At each
-- iteration, the sum of the sequences are incremented by 1.
-- 
-- For each iteration, 
--   - sequences leading to enough distinct solutions are checked to see if one
--     of them doesn't lead to a better solution for n
--   - sequences not leading to enough distinct solutions and whose the
--     corresponding value is not smaller than half the candidate solution
--     are discarded (half because at the next step the value will be multiplied
--     at least by two). 
--   - sequences not leading to enough distinct solutions and whose the
--     corresponding value is smaller than half the candidate solution are kept
--     for the next iteration.
--
-- The iteration stops whenever there is no more candidate exponent sequences.

iter :: Int -> Integer
iter bound = iter' (genExpoSeq exponentsSum) candidate
  where
    exponentsSum = lowerExponentsSum bound
    candidate    = upperBoundForN exponentsSum

    iter' :: [ExpoSeq] -> Integer -> Integer
    iter' ess best
        | null ess = best
        | null ess' = iter' ess''' best
        | otherwise = iter' ess''' best''
      where
        (ess', ess'') = aboveAndBelowBound bound ess
        best' = minimum $ map exponentsToNumber ess'
        best'' = if null ess' then best else min best best'
        ess''' = iterExpoSeqs 
               $ filter (\es -> 2*(exponentsToNumber es)<best'') ess''



-- Return the value corresponding to a sequence of prime exponents
exponentsToNumber :: ExpoSeq -> Integer
exponentsToNumber = product . zipWith (^) primes

main = do
    putStrLn $ "1,000:     " ++ show (iter 1000)
    putStrLn $ "4,000,000: " ++ show (iter (4*10^6))