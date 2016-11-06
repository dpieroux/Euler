import Data.List

--------------------------------------------------------------------------------

type RomanDigit = Char
type RomanNumber = [RomanDigit]

--------------------------------------------------------------------------------

digitValue :: RomanDigit -> Int

digitValue 'I' = 1
digitValue 'V' = 5
digitValue 'X' = 10
digitValue 'L' = 50
digitValue 'C' = 100
digitValue 'D' = 500
digitValue 'M' = 1000

--------------------------------------------------------------------------------

fromRoman :: RomanNumber -> Int
fromRoman roman = fst $ foldr update (0, 0) $ map digitValue roman
  where
    update :: Int -> (Int, Int) -> (Int, Int)
    update val (acc, prev) = if prev <= val then (acc+val, val)
                                            else (acc-val, prev)

--------------------------------------------------------------------------------

toRoman :: Int -> RomanNumber
toRoman n = convert "" n [(1000, "M"), (900, "MC"), (500, "D"), (400, "DC"),
                          ( 100, "C"), ( 90, "CX"), ( 50, "L"), ( 40, "LX"),
                          (  10, "X"), (  9, "XI"), (  5, "V"), (  4, "VI"),
                          (   1, "I")]
  where
    convert :: RomanNumber -> Int -> [(Int, [RomanDigit])] -> RomanNumber
    convert acc n factors@((v, digits):factors')
      | n == 0    = reverse acc
      | v <= n    = convert (digits ++ acc) (n-v) factors
      | otherwise = convert acc              n    factors'

--------------------------------------------------------------------------------

main = do
    fileLines <- readFile "euler_0089.txt"
    let originalNumbers = lines fileLines
    let reworkedNumbers = map (toRoman . fromRoman) originalNumbers
    let originalVal     = length $ concat originalNumbers
    let finalVal        = length $ concat reworkedNumbers
    putStrLn ("Number of characters: ")
    putStrLn ("    Initial: " ++ show originalVal)
    putStrLn ("    Final  : " ++ show finalVal)
    putStrLn ("    Gain   : " ++ show (originalVal - finalVal))

--------------------------------------------------------------------------------

