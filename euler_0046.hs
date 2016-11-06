import Math.NumberTheory.Primes.Testing (isPrime)

-- ds means "doubled square"
-- dss means "doubled squares"
iter :: Int -> [Int] -> [Int] -> Int
iter n smallDss@(ds:_) largeDss@(ds':ds'') 
	| ds' < n + 1                                 = iter n (ds':smallDss) ds''
	| even n || (isPrime.toInteger) n             = iter (n+2) smallDss largeDss 
	| any (isPrime.toInteger) $ map (n-) smallDss = iter (n+2) smallDss largeDss
	| otherwise                                   = n

main = print $ iter 3 [2] [ds | n<-[2..], let ds = 2*n*n]