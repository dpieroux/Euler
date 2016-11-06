import Data.Array;

euler n = v ! (n, (n-1))
  where
    v =accumArray (+) 0 ((0,0), (n, n))
        (  [((0, j), 1) | j<-[1..n]]
        ++ [((i, j), v!(i-j, j)) | i<-[1..n], j<-[1..i] ]
        ++ [((i, j), v!(i, j-1)) | i<-[1..n], j<-[1..n]])

main = print $ euler 100