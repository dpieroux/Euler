{-------------------------------------------------------------------------------

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k; for example, R(6) = 111111.

Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
there always exists a value, k, for which R(k) is divisible by n, and let A(n)
be the least such value of k; for example, A(7) = 6 and A(41) = 5.

The least value of n for which A(n) first exceeds ten is 17.

Find the least value of n for which A(n) first exceeds one-million.

--------------------------------------------------------------------------------

The algorithm is, for increasing values of n, to compute A(n) by building the
smallest repunit R which is a multiple of n.

Details
-------

Because n and 10 are co-prime, the last digit of n can only be 1, 3, 7, 9.

If we consider the product modulo 10 of these four digits with a single-digit
number d, any target digit t can be produced, as shown by the table below:

            +--------+---------------------------------------+
            |  t =   |                   d                   |
            | m * d  +---+---+---+---+---+---+---+---+---+---+
            | mod 10 | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
            +----+---+---+---+---+---+---+---+---+---+---+---+
            |    | 1 | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
            |  m | 3 | 0 | 3 | 6 | 9 | 2 | 5 | 8 | 1 | 4 | 7 |
            |    | 7 | 0 | 7 | 4 | 1 | 8 | 5 | 2 | 9 | 6 | 3 |
            |    | 9 | 0 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 |
            +--- +---+---+---+---+---+---+---+---+---+---+---+

For instance, given m=7, it is possible to produce a target=6 by multiplying m
by 8: 7*6=4_8. Note also that each digit appears only one time in the table:
otherwise said, given a value m, there is one and only way to produce a target
t.

Now, given n, let us derive the smallest R with n|R, by building d such that
n*d=R. If n(i) and d(i) are the digits of n and d, with n0 and d0 the smallest
strength digits. It comes:

    R = n d
      = 10 div(n d, 10) + mod(n d, 10)
      = 10 R(i≥1) + mod(n0 d0, 10)

Therefore, given n and thus n0, d0 is uniquely determined by the condition that
mod(n0 d0, 10) = 1.

For the next digit of d, it comes:
    R(i≥1) = div(n d, 10)
           = n d' + div(n d0, 10)
           = 10 n d'' + n d1 + r0, with the rest r0 = div(n d0, 10)
           = 10 [n d'' + div(n d1 + r0, 10)] + mod(n d1 + r0, 10)

This time d1 is uniquely determined by the condition mod(n d1 + r0, 10) = 1.

It is possible to follow it similarly for all the digits of d, until a rest
becomes 0. This is illustrated on the following example:

    41 * 1      =  41 => d0 = 1, r0 = 4
    41 * 7 + 4  = 291 => d1 = 7, r1 = 29
    41 * 2 + 29 = 111 => d2 = 2, r2 = 11
    41 * 0 + 11 =  11 => d3 = 0, r3 = 1
    41 * 0 +  1 =   1 => d4 = 0, r0 = 0 ==> end

    ==> 5 steps => A(41) = 5

Because there always exists a value, k, for which R(k) is divisible by n, the
algorithm is ensured to stop, and thus all rests must be different, since
otherwise it would loop forever.

Concerning the rests, it comes
    r(0) = div(n*d0, 10)
    r(i) = div(n*d0 + r(i-1), 10), i>0

Let show that r(i)<0 ∀i:
    i=0: r(0) < n since 0 ≤ d0 ≤ 9
    i>0: we assume r(i-1)<n => r(i) = div(n*d0 + r(i-1), 10)
                                    ≤ div(n*d0 + n - 1, 10)
                                    ≤ div(10*n - 1, 10)
                                    < n

So, by recurrence all rest r(i) are smaller than n. Therefore the algorithm has
at most n steps, and thus A(n) ≤ n. For instance, for n=3, the number of step is
3:

    3 * 7     = 21
    3 * 3 + 2 = 11
    3 * 0 + 1 =  1

Therefore to generate R(k) with k>10^6, n must be greater than 10^6.

-------------------------------------------------------------------------------}

import Data.Array.Unboxed
import Data.Array.IArray

import Debug.Trace

type Rest = Int
type Digit = Int

-- Given a rest r, gRestToTarget returns the digit that, when added to r returns
-- either 1 (if r=0 and 1) or 11 (otherwise).
gRestToTarget :: UArray Rest Digit
gRestToTarget = listArray (0, 9) [rem (11 - i) 10 | i <- [0..9]]


-- Given the last digit of n and the rest r from a previous step, returns the
-- next digit of d.
gDigitTargetToDigit :: Array Digit (UArray Rest Digit)
gDigitTargetToDigit = array (0,9)
    [   (1, listArray (0, 9) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    ,   (3, listArray (0, 9) [0, 7, 4, 1, 8, 5, 2, 9, 6, 3])
    ,   (7, listArray (0, 9) [0, 3, 6, 9, 2, 5, 8, 1, 4, 7])
    ,   (9, listArray (0, 9) [0, 9, 8, 7, 6, 5, 4, 3, 2, 1])
    ]

computeA :: Int -> Int
computeA n = computeA' (n*(targetToDigit ! 1) `quot` 10) 1
  where
    n0 = rem n 10
    targetToDigit = gDigitTargetToDigit ! n0

    computeA' :: Int -> Int -> Int
    computeA' rest acc
        = if (rest == 0 && acc /= 0)
            then acc
            else let d = targetToDigit ! (gRestToTarget ! (rem rest 10))
                 in computeA' ((n*d + rest) `quot` 10) (acc+1)

euler :: [Int] -> (Int, Int)
euler (n:ns) = let a = computeA n
               in  if a > 10^6 then (n, a) else euler ns


main = do
    prtln 3 3
    prtln 7 6
    prtln 41 5
    putStrLn $ "A(" ++ show n ++ ") = " ++ show a
  where
    (n, a) = euler [n | i <- [500000..], let n=2*i+1, rem n 5 > 0]
    prtln n res = putStrLn $ "A(" ++ show n ++ ") = " ++ show(computeA n)
            ++ " (should be " ++ show res ++ ")"