{-------------------------------------------------------------------------------

Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.

As n increases, the proportion of bouncy numbers below n increases such that there are only 12951 numbers below one-million that are not bouncy and only 277032 non-bouncy numbers below 10^10.

How many numbers below a googol (10^100) are not bouncy?

--------------------------------------------------------------------------------

There are two mains differences between this problem and the previous one:
    1. The number of numbers to consider (~1.6 10^6 and 10^100)
    2. Here we are asked to count numbers, not to determine a specific number.

The first difference makes the problem impossible to solve with the approach
used for Euler 112. However, the second difference allows for another types of
approach.

Interesting properties of bouncy numbers:
    1. Bouncy numbers appear more frequently for larger numbers than for
       smaller ones. 
    2. If the beginning (in fact any part) of a number is bouncy, then that
       number is bouncy, whatever the rest of the number.
    3. Let's called a number obtained by repeating a same digit a 'constant'
       numbers. Such a number is increasing and decreasing according the
       definitions above.
    4. They are exactly 10 constant numbers of n digits. For instance 0000,
       1111, 2222, ..., 9999 for 4 digits.
    5. Increasing numbers cannot contain the digit 0, while decreasing numbers
       can. There are thus more This is a asymmetry between them.

The approach is to count the number of increasing and decreasing numbers of at
most n digits, to add these two counts, and to remove the constant numbers as
they are counted twice, i.e. 9*d (for 1, 2, ..., 9, 11, 22, ..., 99, 111, 222,
..., 999, etc).

If we call I(d,n) the number of increasing numbers of n digits and starting by d, then we have the recurrence relation: I(d,n) = Sum_(k=d..9) I(d-1, k).

Similarly if we call D(d,n) the number of decreasing numbers of n digits and starting by d, then we have the recurrence relation: I(d,n) = 1+Sum_(k=1..d) I(d-1, k), the "1+" accounting for the number 'd0...0'.

-------------------------------------------------------------------------------}

import Data.Array.IArray

type FirstDigit = Int
type Order = Int -- he number of digits of the number.
type Count = Int

type Table = Array (FirstDigit, Order) Count

-- euler :: Order -> Count
euler order = (nIncNumbers order) + (nDecNumbers order) - 9*order
  
firstDigits = [1..9] 

nIncNumbers :: Order -> Count
nIncNumbers order = sum $ elems $ table 
  where 
    table :: Array (FirstDigit, Order) Count
    table = array ((1, 1), (9, order))
          $ (++) [((d, 1), 1) | d <- firstDigits]
                 [((d, o), n) | d <- firstDigits
                              , o <- [2..order]
                              , let n = sum $ [ table ! (d', o-1)
                                              | d' <- [d..9]]] 

nDecNumbers :: Order -> Count
nDecNumbers order = (sum $ elems $ table)
  where
    table :: Array (FirstDigit, Order) Count
    table = array ((1, 1), (9, order))
          $ (++) [((d, 1), 1) | d <- firstDigits]
                 [((d, o), n) | d <- firstDigits
                              , o <- [2..order]
                              , let n = (+1) $ sum $ [ table ! (d', o-1)
                                                     | d' <- [1..d]]] 
main = do
    putStrLn $ "below 10^6:   " ++ (show $ euler 6)
    putStrLn $ "below 10^10:  " ++ (show $ euler 10)
    putStrLn $ "below 10^100: " ++ (show $ euler 100)