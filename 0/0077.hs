import Data.Array;
import Math.NumberTheory.Primes.Testing(isPrime)

-- The element (i, j) of 'matrix n' is the number of way to write the number i as
-- a sum of primes not greater than j.
matrix n = v where
    v = accumArray (+) 0 ((0, 0), (n, n))
        (  [((0, j), 1)          |            j<-[1..n]]
        ++ [((i, j), v!(i-j, j)) | i<-[1..n], j<-[1..i], isPrime(j)]
        ++ [((i, j), v!(i, j-1)) | i<-[1..n], j<-[1..n]])
{-
-- euler n is the first number that can be written as a sum of more than n primes
euler n = fst $ head
    $ dropWhile (\(i, v) -> v<n)
    $ map (\i -> (i, n_ways i)) [1..]
  where
    n_ways i = matrix i ! (i,i)

-- print the answer to the specific question
main = print $ euler 5000
-}

euler n = fst $ head
    $ dropWhile (\(i, v) -> v<n)
    $ map (\i -> (i, n_ways i)) [1..]
  where
    n_ways i = mat100 ! (i,i)
    mat100 = matrix 100

-- print the answer to the specific question
main = print $ euler 5000
