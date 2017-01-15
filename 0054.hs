import Data.List (nub)
import GHC.Exts (sortWith)

convertValue '2' = 2
convertValue '3' = 3
convertValue '4' = 4
convertValue '5' = 5
convertValue '6' = 6
convertValue '7' = 7
convertValue '8' = 8
convertValue '9' = 9
convertValue 'T' = 10
convertValue 'J' = 11
convertValue 'Q' = 12
convertValue 'K' = 13
convertValue 'A' = 14

type Card = (Int, Char)
type Hand = [Card]

handValue :: Hand -> Int
handValue hand@[(v1, s1), (v2, s2), (v3, s3), (v4, s4), (v5, s5)]
    | isRoyalFlush hand = 10000090000
    | isStraightFlush hand = 10000080000 + v5
    | isFourOfAKindLow hand = 10000070000 + v4
    | isFourOfAKindHigh hand = 10000070000 + v5
    | isFullHouse hand = 10000060000 + v5*100 + v1
    | isFlush hand = 10000050000
    | isStraight hand = 10000040000 + v5
    | isThreeOfAKind hand = 10000030000 + v3
    | isTwoPair = 10000020000 + 100*(pairs !! 1) + (pairs !! 0)
    | isOnePair = 10000010000 + (pairs !! 0)
    | otherwise = v1 + 15*(v2 + 15*(v3 + 15*(v4 + 15*v5)))
  where
    isRoyalFlush hand@[(v1, _), _, _, _, _]                         = v1 == 10 && isFlush hand
    isStraightFlush hand@[(v1, _), _, _, _, (v5, _)]                = isStraight hand && isFlush hand
    isFourOfAKindLow [(v1, _), (v2, _), (v3, _), (v4, _), (v5, _)]  = v1 == v4
    isFourOfAKindHigh [(v1, _), (v2, _), (v3, _), (v4, _), (v5, _)] = v2 == v5
    isFullHouse [(v1, _), (v2, _), (v3, _), (v4, _), (v5, _)]       = (v1 == v2 && v3 == v5) || (v1 == v3 && v4 == v5)
    isFlush [(_, s1), (_, s2), (_, s3), (_, s4), (_, s5)]           = (length $ nub [s1, s2, s3, s4, s5]) == 1
    isStraight [(v1, _), (v2, _), (v3, _), (v4, _), (v5, _)]        = (map (\n -> n-v1) [v2, v3, v4, v5]) == [1, 2, 3, 4]
    isThreeOfAKind [(v1, _), (v2, _), (v3, _), (v4, _), (v5, _)]    = v1 == v3 || v2 == v4 || v3 == v5
    isTwoPair = (length $ nub $ map fst hand) == 3
    isOnePair = (length $ nub $ map fst hand) == 4
    pairs = keepPair $ map fst hand


keepPair (c1:c2:cs) = if c1 == c2 then c1:keepPair cs else keepPair (c2:cs)
keepPair (c:cs) = if elem c cs then c:keepPair cs else keepPair cs
keepPair [] = []


doesPlayer1Win :: (Hand, Hand) -> Bool
doesPlayer1Win (hand1@[_, _, _, _, (v5, _)], hand2@[_, _, _, _, (v5', _)])
    =  (handValue hand1 > handValue hand2)
    || (handValue hand1 == handValue hand2
        && reverse (map fst hand1) > reverse (map fst hand2))


convertCard :: String -> Card
convertCard [value, suit] = (convertValue value, suit)


games :: String -> [(Hand, Hand)]
games textualInput
    = map (sortHands . (splitAt 5) . (map convertCard) . words)
    $ lines $ textualInput


sortHand :: Hand -> Hand
sortHand = sortWith (\(v,s) -> v)


sortHands :: (Hand, Hand) -> (Hand, Hand)
sortHands (h1, h2) = (sortHand h1, sortHand h2)


main = do
    input <- readFile "euler_54.dat"
    print $ length $ filter doesPlayer1Win $ games input