import Data.Ratio

fractions :: [Rational]
fractions = map (1+) fractions'
  where
    fractions' = (1%2) : (map (\q -> 1/(2+q)) fractions')


main = print $ length $ filter predicate $ take 1000 fractions
  where
    predicate q = (length_of $ numerator q) > (length_of $ denominator q)
    length_of   = length.show
