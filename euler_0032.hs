import Data.List (permutations)
import Data.Set (fromList, elems, union)

main = do
    print $ sum $ elems $ fromList ((products s144) ++ (products s234)) where 
        products splitter = [c | (a, b, c) <- map splitter ps, a*b==c]
        s144 ([x1, x2, x3, x4, x5, x6, x7, x8, x9]) = (      x1, ((x2*10+x3)*10+x4)*10+x5, ((x6*10+x7)*10+x8)*10+x9)
        s234 ([x1, x2, x3, x4, x5, x6, x7, x8, x9]) = (x1*10+x2,         (x3*10+x4)*10+x5, ((x6*10+x7)*10+x8)*10+x9)
        ps = permutations [1..9]
