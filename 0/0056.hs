sumDigits acc 0  = acc
sumDigits acc n  = sumDigits (acc+d) n' where (n', d) = quotRem n 10

main = print $ maximum $ map (sumDigits 0) [a^b | a<-[1..99], b<-[1..99]]