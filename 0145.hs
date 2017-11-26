{-------------------------------------------------------------------------------

Some positive integers n have the property that the sum [ n + reverse(n) ]
consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 +
904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are
reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (10⁹)?

--------------------------------------------------------------------------------

The only trick used is that the first digit and the last one must be of
different parity. So, we only search numbers that start by a even digit and end
with an odd one, with all digits combination in the middle.

-------------------------------------------------------------------------------}

import Data.List(foldl')

-- Returns all reversible numbers of n digits that start with a even digit and
-- end with a odd digits.
reversibleNumbers :: Int -> [[Int]]
reversibleNumbers n
  = filter isReversible
  $ concat $ map (\n -> map (:n) evenDigits)  -- add most significant digit
  $ loop (n-2)                                -- add the digit in between
  $ map (\n -> [n]) oddDigits                 -- add least significant digit
  where
    digits = [0..9]
    evenDigits = [2, 4 .. 8]
    oddDigits = [1, 3 .. 9]

    loop :: Int -> [[Int]] -> [[Int]]
    loop 0 acc = acc
    loop i acc = loop (i-1) $ concat $ map (\n -> map (:n) digits) acc

    isReversible ds = isReversible' ds (reverse ds) 0
    isReversible' [] _ c = c == 0 || odd c
    isReversible' (a:as) (b:bs) c
      = let s = a+b+c in if odd s then isReversible' as bs (div s 10) else False

-- Compute the answer for number of up to n digits. The *2 accounts for the fact
-- that reversibleNumbers compute number starting/ending with even/odd digits
-- only.
euler n = 2 * foldl' (\acc n -> acc + (length  $ reversibleNumbers n)) 0 [2..n]

main = do
    putStrLn $ concat $ ["Euler 145, limit=10^3: ", show $ euler 3]
    putStrLn $ concat $ ["Euler 145, limit=10^9: ", show $ euler 9]
