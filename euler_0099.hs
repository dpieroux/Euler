{-------------------------------------------------------------------------------

Comparing two numbers written in index form like 2^11 and 3^7 is not difficult,
as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.

However, confirming that 632382^518061 > 519432^525806 would be much more
difficult, as both numbers contain over three million digits.

Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file
containing one thousand lines with a base/exponent pair on each line, determine
which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example given
above.

-------------------------------------------------------------------------------}

import Data.List

main = do
     -- Reads the file
    input <- readFile "data/base_exp.txt"
    let baseExps = map (\l -> read ('(' : l ++ ")")) $ lines input 
                   :: [(Double, Double)]

    let logs = map (\(b,e) -> e * log b) baseExps

    print $ maximumBy (\(l1, x1) (l2, x2) -> compare x1 x2) $ zip [1..] logs