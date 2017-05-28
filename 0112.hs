{-------------------------------------------------------------------------------

Working from left-to-right if no digit is exceeded by the digit to its left it
is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a
decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a
"bouncy" number; for example, 155349.

Clearly there cannot be any bouncy numbers below one-hundred, but just over half
of the numbers below one-thousand (525) are bouncy. In fact, the least number
for which the proportion of bouncy numbers first reaches 50% is 538.

Surprisingly, bouncy numbers become more and more common and by the time we
reach 21780 the proportion of bouncy numbers is equal to 90%.

Find the least number for which the proportion of bouncy numbers is exactly 99%.

--------------------------------------------------------------------------------

A important property is that if any part of a number is bouncy, then the number
is bouncy. In that sense, the bouncy property is sticky.

A simple finite-state machine describes the evolution of the states of a number
when examined digit by digit. There are 4 states: constant, increasing,
decreasing and bouncy. This leads to the following machine for which a
transition corresponds to examining a new digit.

                     +---+
      +--+           v   |            +--+
      v  |   /---> increasing ---\    v  |
    constant                      -> bouncy
             \---> decreasing ---/
                     ^   |
                     +---+

A brute force approach is good enough here, if determining if a number is bouncy is efficient.

For that we start by examining the least-significant digit and we proceeds
towards the most significant one. We stops as soon as the number is known to be
bouncy.

-------------------------------------------------------------------------------}

isBouncy n = iterConstant lsd rest
  where
    (rest, lsd) = divMod n 10

    iterConstant lsd n
        | n == 0      = False
        | lsd == lsd' = iterConstant   lsd' rest'
        | lsd > lsd'  = iterIncreasing lsd' rest'
        | lsd < lsd'  = iterDecreasing lsd' rest'
      where
        (rest', lsd') = divMod n 10

    iterIncreasing lsd n 
        | n == 0       = False
        | lsd >= lsd'  = iterIncreasing lsd' rest'
        | otherwise    = True
      where
        (rest', lsd') = divMod n 10

    iterDecreasing lsd n
        | n == 0       = False
        | lsd <= lsd'  = iterDecreasing lsd' rest'
        | otherwise  = True
      where
        (rest', lsd') = divMod n 10

euler target = iter 0 1
  where
    iter nBouncy n 
        | (100 * nBouncy') == target * n = n
        | otherwise                      = iter nBouncy' (n+1)
      where
        nBouncy' = if isBouncy n then nBouncy+1 else nBouncy

main = do
    putStrLn $ "50% => " ++ show (euler 50)
    putStrLn $ "90% => " ++ show (euler 90)
    putStrLn $ "99% => " ++ show (euler 99)