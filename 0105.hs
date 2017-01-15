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

Here it looks like the most effective approach is to construct all pairs of
disjoint sets by stopping as soon as it is detected that the set current set is
not SSS.

However, instead of building the subset pairs, we compute directly the difference of their respective sum and of their cardinal.

-------------------------------------------------------------------------------}

import Data.List

type Set = [Int]
type D_Sum = Int  -- Difference of the sums of the element of two subsets
type D_Card = Int -- Difference of the cardinal of two subsets
type Sum = Int
type Length = Int

sigma :: Set -> Int
sigma ls = let s = sum ls in if isSSS ls [] s (length ls) then s else 0

isSSS :: Set -> [(D_Sum, D_Card)] -> Sum -> Length -> Bool
isSSS (l:ls) acc sum card 
  = let subset1 = map (\(s, c) -> (s+l,c+1)) acc
        subset2 = map (\(s, c) -> (s-l,c-1)) acc
        
        isValid (s, c) | c > 0  = s > 0
                       | c < 0  = s < 0
                       | c == 0 = s /= 0 
        
        continue =  (all isValid subset1) 
                 && (all isValid subset2)
        
        acc' = (l, 1) : (-l, -1) : (subset1 ++ subset2 ++ acc)
    
    in if continue then isSSS ls acc' (sum-l) (card-1) 
                   else False

isSSS [] _ _ _ = True

main = do 
    input <- readFile "data/sets.txt"
    let samples = (map (\l -> read $ '[' : l ++ "]") $ lines $ input) :: [Set]
    putStr "Sum of SSS sets: "
    print $ sum $ map sigma samples
