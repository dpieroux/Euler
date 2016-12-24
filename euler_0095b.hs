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

type FactorExp = (Int, Int) -- (b,e) represents b^e
type Prime = Int

-- Returns the prime factorisation of a number
primeFactors :: Int         -- Number to factor
             -> [FactorExp] -- prime factors of the number
primeFactors n = primeFactors' n (map fromIntegral primes) []
  where
    primeFactors' :: Int         -- number to factor
                  -> [Prime]     -- primes
                  -> [FactorExp] -- accumulator
                  -> [FactorExp] -- prime factors of the number
    primeFactors' n (p:ps) acc
        | n == 1       = acc
        | n < p*p      = (n, 1):acc
        | rem n p == 0 = primeFactors' n' ps ((p, exp):acc)
        | otherwise    = primeFactors' n ps acc
      where
        (n', exp) = reduce (n, 0)

        {- reduce (n, a) -> (m, b)  

        Reduces the value n*p^a with regards to the current prime p, i.e. that
        n * p^a = m * p^b, with b the greatest possible value.
        
        As a pecial case: reduce (n, 0) = (m, b) => n = m * p^b
        -}
        reduce :: (Int, Int) -> (Int, Int)
        reduce arg@(n, a) | rem n p == 0 = reduce (quot n p, a+1) 
                          | otherwise    = arg

-- Returns the divisors of a number
divisors :: Int -> [Int]
divisors n = combine $ primeFactors n

expand :: FactorExp -> [Int]
expand (base, exp) = expand' base exp base [1]

expand' base 0 cur acc = acc 
expand' base exp cur acc = expand' base (exp-1) (cur*base) (cur:acc) 

combine :: [FactorExp] -> [Int]
combine factors = combine' factors [1]

combine' :: [FactorExp] -> [Int] -> [Int]
combine' [] acc = acc
combine' (factor:factors) acc = combine' factors (combine'' (expand factor) acc)

combine'' :: [Int] -> [Int] -> [Int]
combine'' factors values = concat $ map (\f -> map (f*) values) factors
     
-- Trajectory
type NodeArray = UArray Int Int

data Trajectory = Trajectory [Int] -- Trajectory
                             (Set.Set Int) -- nodes
                             deriving (Show) 
                             
data Accum = Accum { length_ :: Int 
                   , nodes_ :: [Int]
                   , nodeMemory_  :: NodeArray
                   } deriving (Show) 
                               
data TrajectoryOutcome = Cycle [Int] NodeArray 
                       | NoCycle     NodeArray
                       deriving (Show) 



emptyTrajectory = Trajectory [] Set.empty
emptyAccum :: Int -> Accum
emptyAccum dim = 
    Accum 0 -- length
          [] -- cycles
          (array (1, dim) ((1, 1) : [(i, dim+1) | i <- [2..dim]])) -- visited

step = sum . tail . divisors

trajectory :: Int -> Int -> NodeArray -> TrajectoryOutcome
trajectory n dim nodeMemory
    = trajectory_ n [] nodeMemory 
  where
    trajectory_ :: Int -> [Int] -> NodeArray -> TrajectoryOutcome
    trajectory_ m ms nodeMemory
        | dim < m             = NoCycle nodeMemory
        | nodeMemory ! m < n  = NoCycle nodeMemory
        | nodeMemory ! m == n = Cycle (m:ms) nodeMemory
        | otherwise           = trajectory_ (step m) 
                                            (m:ms)
                                            (nodeMemory // [(m, n)])

getCycle :: [Int] -> [Int]
getCycle nodes = getCycle' (tail nodes) []
  where
    firstNode = head nodes
    
    getCycle' :: [Int] -> [Int] -> [Int]
    getCycle' [] acc = acc
    getCycle' (node:nodes) acc
      | firstNode == node = node:acc
      | otherwise         = getCycle' nodes (node:acc)


run dim = nodes_ $ foldl' iter (emptyAccum dim) [1..dim]
  where
    iter :: Accum -> Int -> Accum
    iter acc@(Accum len cycle nodeMemory) n =
        let 
            trajectoryOutcome = trajectory n dim nodeMemory
        in 
            case trajectoryOutcome of
                NoCycle nodeMemory' -> 
                    acc {nodeMemory_ = nodeMemory'}
                
                Cycle nodes nodeMemory' -> 
                    let cycle' = getCycle nodes
                        len' = length cycle'
                    in 
                        if len < len' then Accum len' cycle' nodeMemory'
                                      else acc {nodeMemory_ = nodeMemory'}
                

main = do 
    let result = run 1000000
    print result
    print $minimum result