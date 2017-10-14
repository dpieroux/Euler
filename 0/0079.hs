import Data.List(nub, sort)

codes = map show $ nub [ 319, 680, 180, 690, 129, 620, 762, 689, 762, 318
                       , 368, 710, 720, 710, 629, 168, 160, 689, 716, 731
                       , 736, 729, 316, 729, 729, 710, 769, 290, 719, 680
                       , 318, 389, 162, 289, 162, 718, 729, 319, 790, 680
                       , 890, 362, 319, 760, 316, 729, 380, 319, 728, 716]

{-
7 is always in first position, 0 in the last one.
Therefore the number has the form 7......0.

Let's remove the occurence of 0 and 7:
-}

codes' = nub $ map (filter (\c-> c /= '0' && c /= '7')) codes

{-
3 is always in first position, 9 in the last one.
Therefore the number has the form 73....90.

Let's remove the occurence of 3 and 9:
-}

codes'' = filter (not . null) $ nub $ map (filter (\c-> c /= '3' && c /= '9')) codes'

{-
1 is always in first position, 8 in the last one.
Therefore the number has the form 731..890.

Let's remove the occurence of 1 and 8:
-}

codes''' = filter (not . null) $ nub $ map (filter (\c-> c /= '1' && c /= '8')) codes''

{-
codes''' is now ["6","2","62"].

So the number is 73162890
-}