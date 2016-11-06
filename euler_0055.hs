--------------------------------------------------------------------------------
-- series n0
--
-- Generate the series (n0, b0), (n1, b1), (n2, b2), ... with
--     b(i) = is n(i) palydrome
--     n(i+1) = n(i) + reversed n(i)
--------------------------------------------------------------------------------
series :: Integer -> [(Integer, Bool)]
series n = (n, s == s') : series (n+n')
  where
    s = show n
    s' = reverse s
    n' = read s'


--------------------------------------------------------------------------------
-- isLychrel n
--
-- Returns true is n is a lynchrel number.
--------------------------------------------------------------------------------

isLychrel n = not $ any snd $ take 50 $ tail $ series n


--------------------------------------------------------------------------------

main = print $ length $ filter isLychrel $ [1..9999]