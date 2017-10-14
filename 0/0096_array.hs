import Data.Array.IArray
import Data.Char
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

data Cell = Value  Int
          | Values (Array Int Bool) Int
    deriving Show

isCellSet :: Cell -> Bool
isCellSet (Value _) = True
isCellSet (Values _ _) = False

--------------------------------------------------------------------------------

type Coord = (Int, Int)
next :: Coord -> Coord
next (8, 8) = (0, 0)
next (i, 8) = (i+1, 0)
next (i, j) = (i, j+1)

linkedWith (i, j) = [(i  ,  j' ) | j' <- [0..8]] ++
                    [(i' ,  j  ) | i' <- [0..8]] ++
                    [(i0 + i',  j0 + j') | let i0 = 3 * (div i 3),
                                           let j0 = 3 * (div j 3),
                                           i' <- [0..2],
                                           j' <- [0..2]]

--------------------------------------------------------------------------------

type Board = Array Coord Cell

--------------------------------------------------------------------------------

set :: Board -> Coord -> Int -> Maybe Board
set board coord val
    = case (board ! coord) of
        Value  v       -> if (v == val) then Just board else Nothing
        Values arr len -> if (arr ! val) then board' else Nothing
  where
    board' = unset (board // [(coord, Value val)]) (linkedWith coord)

    unset :: Board -> [Coord] -> Maybe Board
    unset b [] = (Just b)
    unset b (c:cs)
        | c == coord = unset b cs
        | otherwise  = case (b ! c) of
            Value v        -> if v == val then Nothing else unset b cs
            Values arr len ->
                let
                    arr' =  arr // [(val, False)]
                    b' = set b c (fst $ head  $ filter snd $ assocs (arr'))
                in
                    case () of
                     _| not (arr ! val) -> unset b cs
                      | len == 1        -> Nothing
                      | len == 2        -> case b' of
                                                Just b'' -> unset b'' cs
                                                Nothing  -> Nothing
                      | otherwise       -> unset (b // [(c, Values arr' (len-1))]) cs

--------------------------------------------------------------------------------

fork :: Board -> [Board]
fork board = map fromJust $ filter isJust $ map (\val -> set board pivot val) values
  where
    candidates = [(coord, len) | (coord, Values _ len) <- assocs board]

    pivot = pivot' (9, 9) 10 candidates
      where
        pivot' coord len [] = coord
        pivot' coord len ((coord', len'):ls)
          | len' == 2  = coord'
          | len' < len = pivot' coord' len' ls
          | otherwise  = pivot' coord  len  ls

    Values arr _ =  board ! pivot
    values = map fst $ filter snd $ assocs arr

--------------------------------------------------------------------------------

isDone :: Board -> Bool
isDone board = all isCellSet $ elems board

--------------------------------------------------------------------------------

solve :: Board -> [Board]
solve board = if isDone board then [board]
                              else concat $ map solve $ fork board

--------------------------------------------------------------------------------

mkBoard :: [Int] -> Maybe Board
mkBoard ns
  | length ns == 81 = fst $ foldl' setter (Just nullBoard, (0, 0)) ns
  | otherwise       = error ("mkBoard: wrong input list length: " ++ show ns)

  where
    setter :: (Maybe Board, Coord) -> Int -> (Maybe Board, Coord)

    setter (Nothing, _) _ = (Nothing, (0, 0))

    setter (Just board, coord) val
      | val == 0             = (Just board,            next coord)
      | 1 <= val && val <= 9 = ((set board coord val), next coord)
      | otherwise = error ("mkBoard: wrong input value '" ++ show (val) ++
                           "' at "                        ++ show (coord))
    nullBoard = listArray ((0, 0), (8, 8)) (replicate 81 nullCell)
    nullCell  = Values (listArray (1, 9) (replicate 9 True)) 9

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
showCell (Values _ _) = "."

showFree :: Cell -> String
showFree (Value v)  = "*"
showFree (Values _ len) = show len

--------------------------------------------------------------------------------