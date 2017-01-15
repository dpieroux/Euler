import Data.Array
import Data.List

pSingleRoll :: Array Int Double
pSingleRoll = accumArray (+) 0.0 (2, 12)
    -- [(a+b, 1/36) | a<-[1..6], b<-[1..6], a/=b] -- 6-face dices
    [(a+b, 1/16) | a<-[1..4], b<-[1..4], a/=b] -- 4-face dices

pDoubleRoll :: Array Int Double
pDoubleRoll = accumArray (+) 0.0 (2, 12)
    -- [(2*a, 1/36) | a<-[1..6]] -- 6-face dices
    [(2*a, 1/16) | a<-[1..4]] -- 4-face dices

-- Given a board b, return the probability to be in the case i after a 'nd'
-- double dice rolls.
p b i nd = b!(i, nd)
p16 b i j = p b i j /16

type ProbabilityBoard = Array (Int, Int) Double
-- First index is board case, second index is nbr of double dice rolls

type ProjectedProbabilityBoard = Array Int Double
-- Projection of a ProbabilityBoard, discardig the number of double dice rolls.

cGo = 0
cJl = 10
cG2J = 30
cCH1 = 7
cCH2 = 22
cCH3 = 36
cCCs = [2, 17, 33] -- Community chests
cCHs = [cCH1, cCH2, cCH3]
cSpecials = cG2J : cCCs ++ cCHs

newBoard = accumArray (+) 0 ((0,0), (39, 2))

initBoard :: ProbabilityBoard
initBoard = newBoard [((i, 0), 1.0/40) | i <- [0..39]]

diceIter :: ProbabilityBoard -> ProbabilityBoard
diceIter board = newBoard
    (   [((dest, 0), p board idx nd * pSingleRoll!dice)
          | idx <- [0..39], dice <- [2..12], nd <- [0..2],
            let dest = rem (idx + dice) 40]

    ++  [((dest, nd+1), p board idx nd * pDoubleRoll!dice)
          | idx <- [0..39], dice <- [2, 4..12], nd <- [0, 1],
            let dest = rem (idx + dice) 40]

    ++  [((cJl, 0), p board idx 2 * pDoubleRoll!dice)
          | idx <- [0..39], dice <- [2, 4..12]]
          -- here we clear the number of 'doubles' after three
  )

isSimpleCase idx = not $ elem idx cSpecials

specialIter :: ProbabilityBoard -> ProbabilityBoard
specialIter board = newBoard
    (   [((cJl, 0),    p   board cG2J nd) | nd <- [0..2]] -- go to jail case

    -- Community chest cases
    ++  [((cGo, nd),    p16 board  idx nd) | idx <- cCCs, nd <- [0..2]] -- go to Go
    ++  [((cJl, nd),     p16 board  idx nd) | idx <- cCCs, nd <- [0..2]] -- go to Jail
    ++  [((idx, nd), 14*p16 board  idx nd) | idx <- cCCs, nd <- [0..2]] -- don't move

    -- chance card cases
    ++  [((15,  nd),  2*p16 board cCH1 nd) | nd <- [0..2]] -- go from CH1 to next R (R2)
    ++  [((25,  nd),  2*p16 board cCH2 nd) | nd <- [0..2]] -- go from CH2 to next R (R3)
    ++  [((5,   nd),  2*p16 board cCH3 nd) | nd <- [0..2]] -- go from CH3 to next R (R1)
    ++  [((12,  nd),    p16 board cCH1 nd) | nd <- [0..2]] -- go from CH1 to next U (U1)
    ++  [((28,  nd),    p16 board cCH2 nd) | nd <- [0..2]] -- go from CH2 to next U (U2)
    ++  [((12,  nd),    p16 board cCH3 nd) | nd <- [0..2]] -- go from CH3 to next U (U1)
    ++  [((cGo, nd),    p16 board  idx nd) | idx <- cCHs, nd <- [0..2]] -- go to Go
    ++  [((cJl, nd),    p16 board  idx nd) | idx <- cCHs, nd <- [0..2]] -- go to Jail
    ++  [((5,   nd),    p16 board  idx nd) | idx <- cCHs, nd <- [0..2]] -- go to R1
    ++  [((11,  nd),    p16 board  idx nd) | idx <- cCHs, nd <- [0..2]] -- go to C1
    ++  [((24,  nd),    p16 board  idx nd) | idx <- cCHs, nd <- [0..2]] -- go to E3
    ++  [((39,  nd),    p16 board  idx nd) | idx <- cCHs, nd <- [0..2]] -- go to H2
    ++  [((trg, nd),    p16 board  idx nd) | idx <- cCHs, let trg = idx-3, nd <- [0..2]] -- go back 3 squares
    ++  [((idx, nd),  6*p16 board  idx nd) | idx <- cCHs, nd <- [0..2]] -- don't move

    ++  [((idx, nd),    p   board  idx nd) | idx <- [0..39], isSimpleCase idx, nd <- [0..2]]
    )
  where

turn = specialIter.diceIter

project :: ProbabilityBoard -> ProjectedProbabilityBoard
project board = accumArray (+) 0 (0, 39)
     [(idx, board!(idx, nd)) | idx <- [0..39], nd <- [0..2]]

test n f = sum $ elems $ project $ head $ drop n $ iterate f initBoard
-- Test that the sum of all state probability is 1.

f n    = reverse $ sort $ map (\(a,b) -> (b,a)) $ assocs $ project $ head $ drop n $ iterate turn initBoard

main = print $ map snd $ take 3 $ f 20