{-------------------------------------------------------------------------------

Let S(A) represent the sum of elements in set A of size n. We shall call it a
special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

    (C1)    S(B) /= S(C); that is, sums of subsets cannot be equal.
    (C2)    If B contains more elements than C then S(B) > S(C).

For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because
65 + 87 + 88 = 75 + 81 + 84, whereas {157, 150, 164, 119, 79, 159, 161, 139,
158} satisfies both rules for all possible subset pair combinations and S(A) =
1286.

Using sets.txt, a 4K text file with one-hundred sets containing seven to twelve
elements (the two examples given above are the first two sets in the file),
identify all the special sum sets, A1, A2, ..., Ak, and find the value of S(A1)
+ S(A2) + ... + S(Ak).

NOTE: This problem is related to Problem 103 and Problem 106.

--------------------------------------------------------------------------------

See special_sum_subsets.txt in 'doc\'.

We use the result of section 4.1, including the final remarks.

-------------------------------------------------------------------------------}

import Data.Maybe

-- CardSum is a set place holder, retaining only the set cardinal and the sum of
-- its elements.
type CardSum = (Int, Int)

(<+>) :: Int -> CardSum -> CardSum
(<+>) n (c, s) = (c+1, s+n)

emptyCardSum = [(0, 0)] -- CardSum of the empty set

-- SSSet is the data type for a Special Sum Set. 
data SSSet = SSSet { sum_ :: Int        -- the sum of the elements
                   , subsets_ :: [CardSum] -- the CardSum of its subsets 
                   } deriving Show 

emptySet = SSSet 0 emptyCardSum

addElement :: Int -> SSSet -> Maybe SSSet
addElement n set = if isNothing mergeResult 
                   then Nothing
                   else Just $ SSSet (sum_ set + n) $ fromJust mergeResult

  where
    subsets = subsets_ set
    subsets' = map (n <+>) subsets
    mergeResult = merge subsets subsets' (-1) []

-- Specific sort merge implementation
merge :: [CardSum] -> [CardSum] -> Int -> [CardSum] -> Maybe [CardSum]
merge l1@((c1, s1):l1') l2@((c2, s2):l2') curSum acc
    | s1 <= curSum || s2 <= curSum = Nothing
    -- curSum < s1, s2
    | c1 < c2 = if s1 < s2 then merge l1' l2 s1 ((c1, s1) : acc) 
                           else Nothing
    | c2 < c1 = if s2 < s1 then merge l1 l2' s2 ((c2, s2) : acc) 
                           else Nothing
    -- c1 == c2
    | s1 < s2 = merge l1' l2 s1 ((c1, s1) : acc) 
    | s2 < s1 = merge l1 l2' s2 ((c2, s2) : acc)
    | otherwise = Nothing                                  
merge [] l2 _ acc = Just (reverse acc ++ l2) 

buildSss :: [Int] -> Maybe SSSet
buildSss ns = buildSss' ns emptySet
  where  
    buildSss' :: [Int] -> SSSet -> Maybe SSSet
    buildSss' (n:ns) set = 
      let set' = addElement n set 
      in if isJust set' then buildSss' ns $ fromJust set'
                        else Nothing
    buildSss' [] set = Just set

sssSum :: Maybe SSSet -> Int
sssSum Nothing = 0
sssSum (Just set) = sum_ set

--------------------------------------------------------------------------------

main = do 
    input <- readFile "data/sets.txt"
    let samples = (map (\l -> read $ '[' : l ++ "]") $ lines $ input) :: [[Int]]
    putStr "Sum of SSS sets: "
    print $ sum $ map (sssSum . buildSss) samples
