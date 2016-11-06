import Math.NumberTheory.Primes.Factorisation (factorise)

hasFourDisctinctPrimeFactors :: Int -> Bool
hasFourDisctinctPrimeFactors = (4==) . length . map fst . factorise . toInteger

iter :: Int -> Bool -> Bool -> Bool -> Int
iter n c0 c1 c2
    | c0 && c1 && c2 && c3 = n
    | otherwise            = iter (n+1) c1 c2 c3
    where 
        c3 = hasFourDisctinctPrimeFactors (n+3)
		 
main = print $ iter 1 False False False 