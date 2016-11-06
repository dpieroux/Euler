onDiagonal n = if (odd n) then 4 * sigma (div n 2) 
                          else 4 * sigma (div (n-1) 2) + n
  where sigma n = n*(n+1) `div` 2 

onBorder n = 2*n^2

atOrigin n = n^2

insideLower n = sum [(toLeftUp i j k l) + (toRightDown i j k l) |
                    i <- [2..n], 
                    j <- [1..(i-1)], 
                    let p = gcd i j, 
                    let k = div j p, 
                    let l = div i p]
  where  
    toLeftUp i j k l = min (div i k) (div (n-j) l)
    toRightDown i j k l = min (div (n-i) k) (div j l)

euler n = atOrigin n + onBorder n + onDiagonal n + 2*insideLower n

main = putStrLn $ show $ euler 50 
