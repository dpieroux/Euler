{-------------------------------------------------------------------------------

Let S(A) represent the sum of elements in set A of size n. We shall call it a
special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

    S(B) ≠ S(C); that is, sums of subsets cannot be equal.
    If B contains more elements than C then S(B) > S(C).

If S(A) is minimised for a given n, we shall call it an optimum special sum set.
The first five optimum special sum sets are given below.

n = 1: {1}
n = 2: {1, 2}
n = 3: {2, 3, 4}
n = 4: {3, 5, 6, 7}
n = 5: {6, 9, 11, 12, 13}

It seems that for a given optimum set, A = {a1, a2, ... , an}, the next optimum
set is of the form B = {b, a1+b, a2+b, ... ,an+b}, where b is the "middle"
element on the previous row.

By applying this "rule" we would expect the optimum set for n = 6 to be A = {11,
17, 20, 22, 23, 24}, with S(A) = 117. However, this is not the optimum set, as
we have merely applied an algorithm to provide a near optimum set. The optimum
set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and corresponding
set string: 111819202225.

Given that A is an optimum special sum set for n = 7, find its set string.

NOTE: This problem is related to Problem 105 and Problem 106.

--------------------------------------------------------------------------------

See the file special_sum_subset for more information.

The approach chosen here is to find all SSSs of a given cardinal and whose sum
is smaller than a given value. Then to sort them by their sum and to take the
first one.

-------------------------------------------------------------------------------} 

import Data.List(sortBy, sort)

--------------------------------------------------------------------------------

data IterState 
    = IterState [Int] -- elements computed so far, in decreasing order
                Int   -- sum of elems
                Int   -- cardinal (length) of elems
        deriving Show  

getSSSs :: Int -> Int -> [Int] -> [[Int]] 
getSSSs lvl sigmaMax elems 
    | card == lvl  = [elems]
    | null newSSSs     = []
    | otherwise        = newSSSs
  where
    card = length elems 
    sigma = sum elems
    lowBound = if null elems then 1 else 1 + head elems
    upBound = min upBound1 upBound2
    newAdditionalElems = [lowBound .. upBound]
    newSets = map (:elems) newAdditionalElems
    newSSSs = concat $ map (getSSSs lvl sigmaMax) $ filter (isSSS) newSets

    -- Computation for upBound
    (hElems, lElems') = splitAt ((div card 2)-1) elems
    lElems = if even card then lElems' else tail lElems'
    upBound1 = if card<2 then (maxBound::Int) else sum lElems - sum hElems - 1
    upBound2 = div (sigmaMax - sigma - sum [1..(lvl-card-1)]) (lvl-card)

--------------------------------------------------------------------------------

-- CardSum is a place holder for the subset of a SSS, retaining only the set
-- cardinal and the sum of its elements.
type CardSum = (Int, Int)

(<+>) :: Int -> CardSum -> CardSum
(<+>) n (c, s) = (c+1, s+n)

-- Add an element to a SSS. The result is empty if it is not a SSS.
addElement :: Int -> [CardSum] -> [CardSum]
addElement n cardSums = merge cardSums (map (n <+>) cardSums) (-1) []

-- Specific sort merge implementation of two sorted [CardSum]
-- Return a empty list if the result is not SSS.
merge :: [CardSum] -> [CardSum] -> Int -> [CardSum] -> [CardSum]
merge l1@((c1, s1):l1') l2@((c2, s2):l2') curSum acc
    | s1 <= curSum || s2 <= curSum = []
    -- curSum < s1, s2. 

    -- No need to check for the rule C3b; the build-up of the sets warranties
    -- that the condition is fullfiled.
    | c1 < c2 = merge l1' l2 s1 ((c1, s1) : acc) 
    | c2 < c1 = merge l1 l2' s2 ((c2, s2) : acc) 
    
    -- c1 == c2
    | s1 < s2 = merge l1' l2 s1 ((c1, s1) : acc) 
    | s2 < s1 = merge l1 l2' s2 ((c2, s2) : acc)   
    | otherwise = []                                  
merge [] l2 _ acc = reverse acc ++ l2 

isSSS :: [Int] -> Bool
isSSS ns = isSSS' ns [(0, 0)]
  where  
    isSSS' :: [Int] -> [CardSum] -> Bool
    isSSS' (n:ns) set = 
      let set' = addElement n set 
      in if null set' then False 
                      else isSSS' ns set'
    isSSS' [] set = True

--------------------------------------------------------------------------------

-- Apply the rule given above to extend a sequence of numbers forming a SSS.
extendSequence :: [Int] -> [Int]
extendSequence ns = let b = ns !! (div (length ns) 2 )
                    in b : map (b+) ns 

main = do 
    let sigmaMax = sum $ extendSequence [11, 18, 19, 20, 22, 25]
    let results =  getSSSs 7 sigmaMax []
    let optimum = head $ sortBy (\as bs -> compare (sum as) (sum bs)) results
    print $ concat $ map show $ sort optimum