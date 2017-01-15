-- Here below we don't compute N=p^2+q^3+r^4, but 50e6-N.  

import Data.Set (size, fromList)
import Math.NumberTheory.Primes.Sieve


euler :: Integer -> [Integer] -> [Integer] 
euler 1 rs = rs
euler e rs = euler (e-1) (concat (map rests rs)) 
  where
    rests :: Integer -> [Integer]
    rests limit = takeWhile (0<) [d | p<-primes, let d=limit-p^e]

main = print ((size.fromList) (euler 4 [50*(10^6)]))