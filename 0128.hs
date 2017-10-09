{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}

import Data.Numbers.Primes

data Item = Item {a::Int, b::Int, c::Int, d::Int, n::Int} 
    deriving Show


gItems = iterate f (Item (-1) 0 6 18 3) 
  where
    f (Item a b c d n) = Item b c d (3*n*(n+1)) (n+1)

selectTiles :: [Item] -> [Int]
selectTiles (item:items) = 
    if isPrime(c item-1-b item)
        then b_val ++ c_val ++ (selectTiles items)
        else selectTiles items
  where
    b_val = if (isPrime(c item+1-b item) && isPrime(d item-1-b item))
                then [b item+2]
                else []
    c_val = if (isPrime(c item-1-a item) && isPrime(d item-1-c item))
                then [c item+1]
                else []

gSelectedTiles = 1 : selectTiles gItems

main = do
    putStrLn ("10: "   ++ show (gSelectedTiles!!9))
    putStrLn ("2000: " ++ show (gSelectedTiles!!1999))

