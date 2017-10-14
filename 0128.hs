{-------------------------------------------------------------------------------

A hexagonal tile with number 1 is surrounded by a ring of six hexagonal tiles,
starting at "12 o'clock" and numbering the tiles 2 to 7 in an anti-clockwise
direction.

New rings are added in the same fashion, with the next rings being numbered 8 to
19, 20 to 37, 38 to 61, and so on. The diagram below shows the first three
rings.

                                20
                            21     37
                        22      8      36
                    23      9      19      35
                        10      2      18
                    24      3       7      34
                        11      1      17
                    25      4       6      33
                        12      5      16
                    26      13     15      32
                        27      14     31
                            28     30
                                29


By finding the difference between tile n and each of its six neighbours we shall
define PD(n) to be the number of those differences which are prime.

For example, working clockwise around tile 8 the differences are 12, 29, 11, 6,
1, and 13. So PD(8) = 3.

In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and 10,
hence PD(17) = 2.

It can be shown that the maximum value of PD(n) is 3.

If all of the tiles for which PD(n) = 3 are listed in ascending order to form a
sequence, the 10th tile would be 271.

Find the 2000th tile in this sequence.

--------------------------------------------------------------------------------

Quick facts:
    1) The nth ring has 6n elements
    
    2) So, the nth ring ends with the number 
            last(n) = 1 + 6 + 12 + ... + 6n
                    = 1 + 6 (1 + 2 + ... + n)
                    = 1 + 6 n(n+1)/2
                    = 1 + 3 n(n+1)
    
    3) Thus the nth ring starts with
            start(n) = 1 + end(n-1)
                     = 1 + 1 + 3n(n-1) 
                     = 2 + 3n(n-1) 


There are five type of tiles:
    - type 1: the origin tile 1; PD(1) = 3 since 3-1=2, 4-1=3, 6-1=5
    - type 2a: the tiles starting a ring: 2, 8, 20, ...
    - type 2b: the tiles ending a ring: 7, 19, 37, ...
    - type 3: the tiles that are a corner of a ring (and which are not of type
      2a).
    - type 4: the tiles that are part of a side of a ring (and which are not of
      type 2b).

None of the type-3 tiles can have their PD equal to 3. Indeed, if tile n has
type-3, then    
    - it has 2 adjacent tiles that differ by one unit: n+1 and n-1     
    - it can be shown that the 4 other adjacent tiles split in 2 odd numbers and
      2 even numbers. Therefore the difference between the tile n and each of
      these 4 neighbours must lead to 2 even numbers, which can't be prime. Thus
      PD(n) <= 2.

None of the type-4 tiles can have their PD equal to 3. Indeed if tile n has
type-4, then
    - it has 2 adjacent tiles that differ by one unit: n+1 and n-1
    - the 4 other adjacent tiles are two pairs of adjacent numbers: (a, a+1) and
      (b, b+1). At most one element of each pair can make a prime difference
      with n. Thus here also PD(n) <= 2.

So, only the tiles of types 2 have to be tested.

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

