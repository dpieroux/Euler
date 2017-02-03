{-------------------------------------------------------------------------------

Let S(A) represent the sum of elements in set A of size n. We shall call it a
special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

    S(B) ≠ S(C); that is, sums of subsets cannot be equal.
    If B contains more elements than C then S(B) > S(C).

For this problem we shall assume that a given set contains n strictly increasing
elements and it already satisfies the second rule.

Surprisingly, out of the 25 possible subset pairs that can be obtained from a
set for which n = 4, only 1 of these pairs need to be tested for equality (first
rule). Similarly, when n = 7, only 70 out of the 966 subset pairs need to be
tested.

For n = 12, how many of the 261625 subset pairs that can be obtained need to be
tested for equality?

NOTE: This problem is related to Problem 103 and Problem 105.

--------------------------------------------------------------------------------

Let us first retrieve the number of possible subset pairs. For this we use the
binomial coefficient C(n,k).

n = 4: N = C(4, 1) * C(3, 1) / 2  -- a pair of singleton
         + C(4, 1) * C(3, 2)      -- 1 singleton and 1 pair
         + C(4, 1) * C(3, 3)      -- 1 singleton and 1 triplet
         + C(4, 2) * C(2, 2) / 2  -- a pair of pair
         = 6 + 12 + 4 + 3 = 25

The division by two for sets of equal size comes from the fact that these sets
can be exchanged

n = 7: N = C(7, 1) * [C(6,1) / 2 + C(6,2) + C(6,3) + C(6,4) + C(6,5) + C(6,6)]
         + C(7, 2) * [C(5,2) / 2 + C(5,3) + C(5,4) + C(5,5)]
         + C(7, 3) * [C(4,3) / 2 + C(4,4)]
         = 420 + 441 + 105 = 966

--------------------------------------------------------------------------------

As mentioned in doc/special_sum_subsets.txt, if the second condition holds then
only pairs of of sets of same cardinal must be checked. Thus, in the
expressions above, terms that are not divided by 2 are thus not concerned.

Pairs of sets ({ai}, {bi}) such that ai<bi for all i, or ai>bi for all i
(considering the ai and bi sorted by increasing order) doesn't need to be
checked either. As a special case, singletons don't have to be checked.

Let take n different numbers. We choose 2k numbers from then, which are then
split in two sets of equal cardinal, A and B, such that A contains the smallest
number (to avoid counting twice the same configuration). The number of such
pairs is C(n, 2k)*C(2k-1, k-1).

How many of them need to be checked? In practice we simply needs to consider one
of the configurations of 2k numbers (e.g. [1..2k]), since we only have to check
ai<bi for all i.

If σ(k) is the number of pairs of k-element sets that needs configuration, then
the answer to the question is Sum(k=2.. n/2) C(n, 2k)*σ(k)

-------------------------------------------------------------------------------}

import Data.List

combination n k 
  | 2*k <= n  = product [n-k+1 .. n] `div` product [1 .. k]
  | otherwise = let k' = n-k in product [n-k'+1 .. n] `div` product [1 .. k']

euler n = sum [combination n (2*k) * sigma k | k <- [2.. (div n 2)]]

{-------------------------------------------------------------------------------

To compute sigma k, we consider all the way to split [1..2k] in two sublists a
and b of length n, with a1=1, and then we count those for which it is not true
that ai < bi for all i.

-------------------------------------------------------------------------------}

split (e:es) = filter (\(l, r) -> k == length l)
             $ split' es [([e], [])]
  where
    k = div (1 + length es) 2 
    split' es acc = foldl' update acc es

    update :: [([Int], [Int])] -> Int -> [([Int], [Int])]
    update acc e = map (\(l, r) -> ((e:l), r)) acc 
                 ++ map (\(l, r) -> (l, (e:r))) acc


sigma k = length $ filter (\ls -> any (<0) ls) 
         $ map (\(l, r) -> zipWith (-) r l)
         $ split [1..2*k]
