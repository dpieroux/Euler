import Math.NumberTheory.Primes.Sieve(primes)
import Data.List(maximumBy)

intPrimes :: [Int]
intPrimes = map fromIntegral primes

primesProd = (head primes) : zipWith (*) primesProd (tail primes)

main = print $ last $ takeWhile (<=1000000) primesProd