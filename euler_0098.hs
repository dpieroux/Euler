{-------------------------------------------------------------------------------
By replacing each of the letters in the word CARE with 1, 2, 9, and 6
respectively, we form a square number: 1296 = 36^2. What is remarkable is that,
by using the same digital substitutions, the anagram, RACE, also forms a square
number: 9216 = 962. We shall call CARE (and RACE) a square anagram word pair and
specify further that leading zeroes are not permitted, neither may a different
letter have the same digital value as another letter.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
containing nearly two-thousand common English words, find all the square anagram
word pairs (a palindromic word is NOT considered to be an anagram of itself).

What is the largest square number formed by any member of such a pair?

NOTE: All anagrams formed must be contained in the given text file.

-------------------------------------------------------------------------------}

import Data.List
import qualified Data.Map.Strict as Map

-- Returns the partition of a list into sublists, by considering that two
-- elements are equivalent if their image under the given function is equal.
partitionWith :: Ord b => (a -> b) -> [a] -> [[a]]
partitionWith f ls 
  = Map.elems $ foldl' insert' Map.empty ls 
    where
      insert' map item = Map.insertWith (++) (f item) [item] map


-- A pair of Anagrams. 'min_' comes before 'max_' in lexicographic order. 'len_'
-- is equal to the length of the anagrams and is stored for efficiency.
data AnagramPair = AnagramPair {len_ :: Int, 
                                min_ :: String, 
                                max_ :: String}
                   deriving (Show)


-- AnagramPair constructor. 
newAnagramPair x y = if (x < y) 
                     then AnagramPair (length x) x y 
                     else AnagramPair (length x) y x 


-- Returns a list of AnagramPairs from a list of words. 
anagrams :: [String] -> [AnagramPair]
anagrams = concat 
         . map makePairs
         . partitionWith sort
  where
    makePairs (x:ys) = [newAnagramPair x y | y <- ys] ++ makePairs ys
    makePairs []     = []

-- Returns the Cartesian product of two lists a and b. The order of the elements
-- is preserved. The first element of a is first associated with each element of
-- b, then the second element of a is associated with each element of b, etc.
cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct as bs = [(a, b) | a <- as, b <- bs]


-- Check if two pairs of anagrams are compatible, i.e. if the second one can be
-- retrieved from the first one under a bijective mapping of the characters.
areCompatible :: AnagramPair -> AnagramPair -> Bool
areCompatible (AnagramPair l n1 n2) (AnagramPair l' w1 w2) 
  = (l == l') && (match n12 (w1++w2) Map.empty || match n12 (w2++w1) Map.empty)
  where
    n12 = n1 ++ n2

    match :: String -> String -> Map.Map Char Char -> Bool
    match (a:as) (b:bs) acc
        = case val_a of
              Nothing -> match as bs (Map.insert a b acc)
              Just v  | v == b    -> match as bs acc
                      | otherwise -> False
      where 
        val_a = Map.lookup a acc

    match [] [] acc = noDuplicate $ Map.elems acc
      where
        noDuplicate (l:ls) = not (elem l ls) && noDuplicate ls
        noDuplicate [] = True


main = do
    -- Reads the file
    input <- readFile "data/words.txt"
    let words = read ('[' : input ++ "]") :: [String]

    -- List of the anagram pairs of the words
    let wordAnagrams = anagrams words
                         
    -- List of the anagrams pairs of the squares
    let squareAnagrams = anagrams $ map show squares
                           where
                             bound = (10^) $ maximum $ map len_ wordAnagrams
                             squares = takeWhile (<bound) $ [n*n | n <- [1..]]

    -- All matching pair of word and square anagrams
    let solution = maximum 
                 $ map (read . max_ . fst)
                 $ filter (\(x, y) -> areCompatible x y)
                 $ cartesianProduct squareAnagrams wordAnagrams
                 :: Int

    putStr("Euler 98 solution: ")
    print solution
