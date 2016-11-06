import Data.Array.IArray
import Data.Char
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

data Cell = Value  Int
          | Values [Int]
    deriving Show

isCellSet :: Cell -> Bool
isCellSet (Value _) = True
isCellSet (Values _) = False

--------------------------------------------------------------------------------

type Coord = (Int, Int)
next :: Coord -> Coord
next (8, 8) = (0, 0)
next (i, 8) = (i+1, 0)
next (i, j) = (i, j+1)

linkedWith (i, j) = [(i  ,  j' ) | j' <- [0..8], j /= j'] ++        
                    [(i' ,  j  ) | i' <- [0..8], i /= i'] ++
                    [(i'',  j'') | let i0 = 3 * (div i 3),
                                   let j0 = 3 * (div j 3),
                                   i' <- [0..2], let i'' = i0 + i', 
                                   j' <- [0..2], let j'' = j0 + j', 
                                   i /= i'' && j /= j'']

--------------------------------------------------------------------------------

type Board = Array Coord Cell

--------------------------------------------------------------------------------

set :: Board -> Coord -> Int -> Maybe Board
set board coord val 
    = case cell of 
        Value  v  -> if val == v    then Just board  else Nothing
        Values vs -> if elem val vs then board'      else Nothing
  where
    cell = board ! coord
    board' = unset (board // [(coord, Value val)]) (linkedWith coord) 
                              
    unset :: Board -> [Coord] -> Maybe Board 
    unset board [] = (Just board)
    unset board (coord:coords) = case (board ! coord) of
        Value v     -> if v == val      then Nothing else unset board coords
        Values vals -> if vals == [val] then Nothing else unset (board // [(coord, Values (delete val vals))]) coords

--------------------------------------------------------------------------------

reduce :: Board -> Maybe Board
reduce board = reduce' board (0, 0) 0
  where
    reduce' :: Board -> Coord -> Int -> Maybe Board
    reduce' board _ 81 = Just board
    reduce' board coord iter = case (board ! coord) of 
        Values [val] -> 
            let board' = set board coord val in 
                if isNothing board' then Nothing 
                                    else reduce' (fromJust board') (next coord) 0
        otherwise -> reduce' board (next coord) (iter+1)

--------------------------------------------------------------------------------

fork :: Board -> [Board]
fork board = map fromJust $ filter isJust $ map (\val -> set board pivot val) values    
  where
    candidates = [(coord, length vals) | (coord, Values vals) <- assocs board]
    
    pivot = pivot' (9, 9) 10 candidates 
      where
        pivot' coord len [] = coord
        pivot' coord len ((coord'@(i, j), len'):ls)
          | len' == 2  = coord'
          | len' < len = pivot' coord' len' ls
          | otherwise  = pivot' coord  len  ls
    
    Values values = board ! pivot 

--------------------------------------------------------------------------------

isDone :: Board -> Bool
isDone board = all isCellSet $ elems board 

--------------------------------------------------------------------------------

solve :: Board -> [Board]
solve board 
  | isNothing mbBoard = []  
  | isDone    board'  = [board']
  | otherwise         = concat $ map solve $ fork board'
  where
    mbBoard = reduce board 
    board' = fromJust mbBoard

--------------------------------------------------------------------------------

mkBoard :: [Int] -> Maybe Board
mkBoard ns  
  | length ns == 81 = fst $ foldl' setter (Just nullBoard, (0, 0)) ns
  | otherwise       = error ("mkBoard: wrong input list length: " ++ show ns) 
 
  where
    setter :: (Maybe Board, Coord) -> Int -> (Maybe Board, Coord)

    setter (Nothing, _) _ = (Nothing, (0, 0)) 

    setter (Just board, coord) val
      | val == 0             = (Just board,          next coord)
      | 1 <= val && val <= 9 = ((set board coord val), next coord)
      | otherwise = error ("mkBoard: wrong input value '" ++ show (val) ++ 
                           "' at "                        ++ show (coord))
    nullBoard = listArray ((0, 0), (8, 8)) (replicate 81 (Values [1..9]))

--------------------------------------------------------------------------------

convert :: String -> [Maybe Board]
convert str = convert' $ filter (\s -> head s /= 'G') $ (lines str)
  where
    convert' :: [String] -> [Maybe Board]
    convert' [] = []
    convert' strs = let (h, t) = (splitAt 9 strs) in (mkBoard $ map digitToInt $ concat h) : convert' t  

--------------------------------------------------------------------------------

euler :: [Board] -> Int
euler boards = foldl' (\acc board -> euler1 board + acc) 0 boards 
  where
    euler1 :: Board -> Int
    euler1 board = 100 * val1 + 10 * val2 + val3
      where
        Value val1 = board!(0, 0)
        Value val2 = board!(0, 1)
        Value val3 = board!(0, 2)

--------------------------------------------------------------------------------

main = do
    file <- readFile "z:/euler/euler_0096.txt"
    putStrLn $ show $ euler $ concat $ map solve $ map fromJust $ filter isJust $ convert file

--------------------------------------------------------------------------------
-- Debugging functions
--------------------------------------------------------------------------------

showBoard :: (Array Coord String) -> String
showBoard board 
    = concat [concat [(board ! (i, j)) ++ (if j == 8 then "\n" else " ") | j <- [0..8]] | i <- [0..8]]

showBoard' :: Maybe Board -> String
showBoard' Nothing = "Nothing\n"
showBoard' (Just board) = showBoard $ amap showCell board

showCell :: Cell -> String
showCell (Value v) = show v 
showCell (Values _) = "."

showFreedom :: Cell -> String
showFreedom (Value v)  = "*"
showFreedom (Values vs) = show $ length vs

--------------------------------------------------------------------------------