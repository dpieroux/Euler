{-
================================================================================
The proper divisors of a number are all the divisors excluding the number
itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the
sum of these divisors is equal to 28, we call it a perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum of the
proper divisors of 284 is 220, forming a chain of two numbers. For this reason,
220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with 12496, we
form a chain of five numbers:

12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)

Since this chain returns to its starting point, it is called an amicable chain.

Find the smallest member of the longest amicable chain with no element
exceeding one million.
================================================================================
-}

import Data.List
import Data.Numbers.Primes (primes)
import qualified Data.Set as Set
import Data.Array.Unboxed

type Base = Int
type Exp = Int
type Factor = (Base, Exp)
type Prime = Int

-- Returns the prime factorisation of a number
primeFactors :: Int -> [Factor] 
primeFactors n = primeFactors' n (map fromIntegral primes) []
 
primeFactors' :: Int -> [Prime] -> [Factor] -> [Factor]
primeFactors' n (p:ps) acc
    | n == 1       = acc
    | n < p*p      = (n, 1):acc
    | rem n p == 0 = primeFactors' n' ps ((p, exp):acc)
    | otherwise    = primeFactors' n ps acc
  where
    (n', exp) = reduce (div n p) p 1

reduce n p exp
    | r == 0 = reduce q p (exp+1)
    | otherwise = (n, exp)
  where
    (q, r) = quotRem n p

-- Returns the divisors of a number
divisors :: Int -> [Int]
divisors n = combine $ primeFactors n

expand :: Factor -> [Int]
expand (base, exp) = expand' base exp base [1]

expand' base 0 cur acc = acc 
expand' base exp cur acc = expand' base (exp-1) (cur*base) (cur:acc) 

combine :: [Factor] -> [Int]
combine factors = combine' factors [1]

combine' :: [Factor] -> [Int] -> [Int]
combine' [] acc = acc
combine' (factor:factors) acc = combine' factors (combine'' (expand factor) acc)

combine'' :: [Int] -> [Int] -> [Int]
combine'' factors values = concat $ map (\f -> map (f*) values) factors
     
-- Trajectory
data Trajectory = Trajectory [Int] -- Trajectory
                             (Set.Set Int) -- nodes
                             deriving (Show) 
                             
data Accum = Accum { minVal_ :: Int
                   , length_ :: Int 
                   , cycle_ :: [Int]
                   , nodeArray_  :: (UArray Int Bool) -- visited
                   } deriving (Show) 
                               
data TrajectoryOutcome = Cycle [Int] -- nodes
                               [Int] -- cycle
                       | Failure [Int] -- nodes
                       deriving (Show) 



emptyTrajectory = Trajectory [] Set.empty
emptyAccum :: Int -> Accum
emptyAccum dim = 
    Accum 0 -- minVal 
          0 -- length
          [] -- cycles
          (array (1, dim) ((1, True) : [(i, False) | i <- [2..dim]])) -- visited

step = sum . tail . divisors

trajectory :: Int -> Int -> Accum -> TrajectoryOutcome
trajectory n dim (Accum minVal len cycle nodeArray) 
    = trajectory_ n emptyTrajectory 
  where
    trajectory_ :: Int -> Trajectory -> TrajectoryOutcome
    trajectory_ n (Trajectory trajectory nodes)
        | dim < n            = Failure trajectory
        | nodeArray ! n      = Failure trajectory
        | Set.member n nodes = Cycle trajectory (cycle trajectory [])
        | otherwise          = trajectory_ (step n) 
                                           (Trajectory (n:trajectory) 
                                                       (Set.insert n nodes))
      where
        cycle :: [Int] -> [Int] -> [Int]
        cycle (node:nodes) acc
          | n == node = node:acc
          | otherwise = cycle nodes (node:acc)


run dim = cycle_ $ foldl' iter (emptyAccum dim) [1..dim]
  where
    iter :: Accum -> Int -> Accum
    iter acc n =
        let 
            trajectoryOutcome = trajectory n dim acc
        in 
            case trajectoryOutcome of
                Cycle nodes cycle -> 
                    let nodeArray' = (nodeArray_ acc) // [(i,True) | i <- nodes]
                        len' = length cycle
                        minVal' = minimum cycle
                    in 
                        if (length_ acc) < len' then Accum minVal' len' cycle nodeArray'
                                                else acc {nodeArray_ = nodeArray'}
                Failure nodes -> acc {nodeArray_ = (nodeArray_ acc) // [(i,True) | i <- nodes]}

main = do 
    let result = run 1000000
    print result
    print $minimum result