import Data.Ratio

haley n = iter 1 n 1 (n-1)
  where
  	iter a b c d = (a, b) : iter c d p q
  	  where
  		x = div (n+b) d
  		p = x*c-a
  		q = x*d-b

euler = takeWhile (\(a, b) -> 2*a < b)
      $ dropWhile (\(a,b) -> 3*a <= b)
      $ haley 12000

main = print $ length euler