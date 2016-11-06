isPentagonal :: Int -> Bool
isPentagonal p = let n = round ((1+sqrt(1+24*fromIntegral p))/6) in 2*p == n*(3*n-1)

hexagonal :: Int -> Int
hexagonal n = n*(2*n-1)

theChosen = [h | n<-[1..], let h=hexagonal n, isPentagonal h]

main = print (take 3 theChosen)