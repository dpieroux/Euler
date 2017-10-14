import Data.List (foldl1')
import Data.Ratio ((%), denominator)

main = do
    print $ denominator $ foldl1' (*) fractions where
        fractions = 
            [a%b | a <- [1..8], b <-[(a+1)..9], (n,r) <- [divMod (9*a*b) (10*b-a)], r==0, n<10] ++
            [a%b | a <- [1..8], b <-[(a+1)..9], (n,r) <- [divMod (9*a*b) (10*a-b)], r==0, n<10]
