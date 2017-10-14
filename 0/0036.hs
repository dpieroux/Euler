-- Turn a number into its digits 
digitsBase _ 0 = []
digitsBase b n = d : digitsBase b n' where (n', d) = divMod n b

-- Check if a list is palidromic
isPalidromic ls = ls == reverse ls

main = do
    print $ sum [n | n <- [1, 3 .. (10^6-1)], 
                 isPalidromic (digitsBase 10 n), 
                 isPalidromic (digitsBase 2 n)]