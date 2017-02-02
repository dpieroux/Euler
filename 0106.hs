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

In addition, pairs of singletons don't have to be checked because such a
singleton contains a different element from the other one.

Let consider two subsets of n elements {ai} and {bi} with i=1..n. If ai<bi for
each i, then there is no need to check either, because it is then clear that the
sum of the ai will be smaller than the sum of the bi.

Given N elements, There are  C(N,n)*C(N-n,n)/2 distinct pairs of ({ai}, {bi}} of
n elements, with a1 being the the smallest elements of all.

Rational: we select n elements out of N to be part of {ai}, and then n elements
out of the N-n left for {yi}. We sort the xi and the yi and we drop the pairs
for which y1 is the smallest number.
