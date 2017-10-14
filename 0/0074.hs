import Data.Array

factoChar :: Array Char Int
factoChar = array ('0', '9') (('0' , 1):[(d2c i, i*factoChar!(d2c (i-1))) | i <- [1..9]])
  where
  	d2c = head.show

step :: Int -> Int
step n = sum $ map (\d -> factoChar ! d) $ show n

seqEuler :: Int -> [Int]
seqEuler n = iter (step n) [n]
  where
  	iter n acc = if (elem n acc) then acc else iter (step n) (n:acc)

validNumbers = filter ((60 ==) . length . seqEuler)
	[((((a*10+b)*10+c)*10+d)*10+e)*10+f |
		a <- [0..9], b <- [0..9], c <- [0..9],
		d <- [0..9], e <- [0..9], f <- [0..9]]

main = print $ length validNumbers