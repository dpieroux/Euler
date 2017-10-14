import Data.Set

--------------------------------------------------------------------------------
-- rests q d 
--
-- Returns the infinite sequence of the rests of the division of q by d. 
--------------------------------------------------------------------------------

rests :: Int -> Int -> [Int]
rests q d = q' : rests (10 * q') d  where q' = mod q d


--------------------------------------------------------------------------------
-- fstRepeated xs
--
-- Returns the first element of xs that is repeated. Note: it is assumed that xs
-- contains repeated elements.
--------------------------------------------------------------------------------

fstRepeated :: [Int] -> Int
fstRepeated xs = fstRepeated' xs empty 
    where 
        fstRepeated' :: [Int] -> Set Int -> Int
        fstRepeated' (x:xs) acc 
            | member x acc = x 
            | otherwise  = fstRepeated' xs (insert x acc)


--------------------------------------------------------------------------------
-- recurringCycle d
--
-- Returns the recurring cycle of the rests of 1/d.
--------------------------------------------------------------------------------

recurringCycle:: Int -> [Int]
recurringCycle d = r : takeWhile (/= r) (tail (dropWhile (/= r) rests'))
    where 
        rests' = rests 1 d
        r = fstRepeated rests'


--------------------------------------------------------------------------------
-- euler_23 n
--
-- Returns the value d such that 1/d has the longest recurring cycle of all 1/m
-- for m = 1 .. n.
--------------------------------------------------------------------------------

euler_23 :: Int -> Int
euler_23 n = euler_23' n 0 0 where
    euler_23' :: Int -> Int -> Int -> Int
    euler_23' i d l_d
        -- i is the current number to be tested
        -- d is the current candidate number 
        -- l is the length of the recurring cycle of 1/d
        
        | i < l_d     = d 
        -- The length of the cycle of 1/i being i at most, it will not be
        -- possible to get a better candidate than d as i is decreasing. 
        
        | l_d <= l_i   = euler_23' (i-1) i l_i
        -- The recurring cycle of 1/i is longer than the one of 1/d: i is the
        -- new candidate and we test i-1.

        | otherwise = euler_23' (i-1) d l_d
        -- The recurring cycle of 1/i is not longer than the one of 1/d. Thus
        -- we keep d and we test i-1.

        where l_i = length (recurringCycle i)


--------------------------------------------------------------------------------

main = do
    print (euler_23 999)