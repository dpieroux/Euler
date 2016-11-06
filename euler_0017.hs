-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
-- 20 letters. The use of "and" when writing out numbers is in compliance with
-- British usage.

--------------------------------------------------------------------------------

convert99 :: Int -> String
convert99 n
    | n < 20    = upto19 !! (n-1)
    | u == 0    = tenty
    | otherwise = tenty ++ "-" ++ unit
  where
    upto19 = ["one", "two", "three", "four", "five", "six", "seven", "eight",
              "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
              "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

    tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety"]

    (t, u) = quotRem n 10
    tenty = tens !! (t-2)
    unit  = upto19 !! (u-1)

--------------------------------------------------------------------------------

convert999 :: Int -> String
convert999 n
    | n < 100   = convert99 n
    | t == 0    = hundreds ++ " hundred"
    | otherwise = hundreds ++ " hundred and " ++ tens
  where
    (h, t) = quotRem n 100
    hundreds = convert99 h
    tens     = convert99 t

--------------------------------------------------------------------------------

convert :: Int -> String
convert n
    | n < 1000  = convert999 n
    | h == 0    = thoudands ++ " thousand"
    | otherwise = thoudands ++ " thousand" ++ " " ++ hundreds
  where
    (t, h) = quotRem n 1000
    thoudands = convert999 t
    hundreds  = convert999 h
--------------------------------------------------------------------------------

nbrLetters :: String -> Int
nbrLetters str = foldr update 0 str
  where
    update c acc = if 'a' <= c && c <= 'z' then acc+1 else acc

--------------------------------------------------------------------------------

euler n = sum $ map (nbrLetters . convert) [1 .. 1000]

--------------------------------------------------------------------------------

main = do
    print $ nbrLetters $ convert 342
    print $ nbrLetters $ convert 115
    print $ sum $ map (nbrLetters . convert) [1 .. 1000]