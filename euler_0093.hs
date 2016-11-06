import Data.List
import Data.List.Unique
import Data.Ratio
import Data.Maybe

type MaybeRat = Maybe (Ratio Integer)

(+?) (Just a) (Just b) = Just (a + b)
(+?) _ _ = Nothing

(*?) (Just a) (Just b) = Just (a * b)
(*?) _ _ = Nothing

(-?) (Just a) (Just b) = Just (a - b)
(-?) _ _ = Nothing

(-!?) (Just a) (Just b) = Just (b - a)
(-!?) _ _ = Nothing

(/?)        _ (Just 0) = Nothing
(/?) (Just a) (Just b) = Just (a / b)
(/?) _ _ = Nothing

(/!?) (Just 0)        _ = Nothing
(/!?) (Just a) (Just b) = Just (b / a)
(/!?) _ _ = Nothing

ops :: [MaybeRat -> MaybeRat -> MaybeRat]
ops = [(+?), (*?), (-?), (-!?), (/?), (/!?)]

expressions = [(\[a, b, c, d] -> op1 a (op2 b (op3 c d))) | op1 <- ops, op2 <- ops, op3 <- ops]
          ++  [(\[a, b, c, d] -> op1 (op2 a b) (op3 c d)) | op1 <- ops, op2 <- ops, op3 <- ops]

firstMissing n [] = n
firstMissing n (l:ls) = if (n==l) then firstMissing (n+1) ls else n

values =
    [(  firstMissing 1 $ 
        sortUniq $ 
        map numerator $ 
        filter (\q -> 0<q && 1 == denominator q) $ 
        map (\(Just q) -> q) $
        filter (/= Nothing) $ 
        concat $ map (\expr -> map expr perms) expressions     
     ,
        d+10*(c+10*(b+10*a))
     )
        | a<-[1..6], b<-[a+1..7], c<-[b+1..8], d<-[c+1..9], 
          let perms = permutations [Just (a%1), Just (b%1), Just (c%1) , Just (d%1)] :: [[MaybeRat]]
    ] 

main = putStrLn $ show $ maximum values 
-- This assumes that there is only one value that generates the greatest sequence. Otherwise the largest
-- will be returned.

