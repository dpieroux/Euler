import Data.Set

minimumNumber k = search 2 k (k+1) 2 (2*k) k
  where
    search value index sum prod best bound =
        if (index == 0) || (value > bound) || (sum >= best) || (prod >= best) 
        then best
        else min (search 2         (index-1) (sum+1) (prod*2)                       best' value)
                 (search (value+1) index     (sum+1) ((value+1) * (div prod value)) best' bound)
      where 
        best' = if (sum == prod) && (sum<best) then sum else best

uniqueNumbers upto = toList $ fromList [minimumNumber k | k<-[2..upto]]

euler upto = sum $ uniqueNumbers upto

main = print $ euler 12000 
