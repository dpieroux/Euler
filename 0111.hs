{-------------------------------------------------------------------------------

Considering 4-digit primes containing repeated digits it is clear that they cannot all be the same: 1111 is divisible by 11, 2222 is divisible by 22, and so on. But there are nine 4-digit primes containing three ones:

        1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111

We shall say that M(n, d) represents the maximum number of repeated digits for an n-digit prime where d is the repeated digit, N(n, d) represents the number of such primes, and S(n, d) represents the sum of these primes.

So M(4, 1) = 3 is the maximum number of repeated digits for a 4-digit prime where one is the repeated digit, there are N(4, 1) = 9 such primes, and the sum of these primes is S(4, 1) = 22275. It turns out that for d = 0, it is only possible to have M(4, 0) = 2 repeated digits, but there are N(4, 0) = 13 such cases.

In the same way we obtain the following results for 4-digit primes.
        d    M(4, d)     N(4, d)     S(4, d)
        0       2           13        67061
        1       3            9        22275
        2       3            1         2221
        3       3           12        46214
        4       3            2         8888
        5       3            1         5557
        6       3            1         6661
        7       3            9        57863
        8       3            1         8887
        9       3            7        48073

For d = 0 to 9, the sum of all S(4, d) is 273700.

Find the sum of all S(10, d).

--------------------------------------------------------------------------------

Generating all the primes of 10 digits takes a long time. Therefore, that brutal
approach is probably not the right one.

By looking to the table, all M(4, d) are equal to 3, excepted 0. So, this may be
a hint that for a higher number n of digits, the situation will be identical:
there will be primes with (n-1) identical digits.

Concerning 0, the first digit must be different than 0 and the last one too
(otherwise the number would be divisible by 10). So, this is an invitation to look for primes of the form a,000,000,00b. And indeed, there are such prime numbers: 
> filter isPrime [a*10^9+b | a <- [1..9], b <- [1, 3, 7, 9]]
[1000000007,1000000009,4000000007,4000000009,
 6000000001,6000000007,7000000001,9000000001]

Thus M(10, 0) = 8 and N(10, 0) = 8

-------------------------------------------------------------------------------}

import Data.Numbers.Primes

--------------------------------------------------------------------------------
-- Prime numbers with of n digits, with n-2 digits '0'.
--------------------------------------------------------------------------------

primes_0 n = filter isPrime [a*10^(n-1)+b | a <- [1..9], b <- [1, 3, 7, 9]]

--------------------------------------------------------------------------------
-- Generate all combinaisons made of a star '*' and of (n-1) dots '.'
--
-- Ex: getCombinaisons 4 = ["...*","..*.",".*..","*..."]
--------------------------------------------------------------------------------

genCombinaisons n = iter (take (n-1) $ repeat '.') [[]]
  where
    iter :: [Char] -> [[Char]] -> [[Char]]
    iter es@(e:es') acc = acc'++acc''
      where
        acc' = let x = reverse ('*' : es) in map (x ++) acc
        acc'' = iter es' $ map (e:) acc

    iter [] acc = map ('*':) acc


--------------------------------------------------------------------------------
-- Generate all combinaisons of n digits (represented as chars) with (n-1)
-- identical digits
--
-- Ex: patterns 4 = ["1110","1101","1011","0111","2220", ..., "8988","9888"]
--------------------------------------------------------------------------------

patterns n = concat $ map genNumbers [(star, dot) | star <- ['0'..'9']
                                                   , dot  <- ['0'..'9']
                                                   , star /= dot
                                      ]
  where 
    patterns = genCombinaisons n
    genNumbers (star, dot) = map (map reify) patterns
      where
        reify '*' = star
        reify '.' = dot

--------------------------------------------------------------------------------
-- Generate all combinaisons of n digits (represented as chars) with (n-1)
-- identical digits that are candidate as a prime number.
--
-- Ex: patterns 4 = ["1011","1101","3033","3303","7077", ..., "7779","8889"]
--------------------------------------------------------------------------------

validPatterns n = ps'''
  where
    -- ps are the candidate patterns in reverse order
    ps = patterns n

    -- The patterns not ending (i.e. starting seen the reverse order) by 1, 3, 7
    -- or 9 are discared:
    ps' = filter (\p -> elem (head p) "1379") ps

    -- Get the pattern in the right order
    ps'' = map reverse ps'

    -- The pattern must start with a non '0' digit
    ps''' = filter (\p -> head p /= '0') ps''

--------------------------------------------------------------------------------
-- Return the prime made of n digits with (n-1) identical digits
--
-- Ex: specialPrimes 10 
-- * [3333133333,6666666661,1777777777,7777717777,9199999999,1121111111,
-- *  1111211111,1111111121,3233333333,3333323333,3333332333,3333333323,
--    2777777777,7727777777,7777772777,9299999999,9999929999,9999999929,
--    1111411111,1111111411,3334333333,7777747777,9999499999,1151111111,
--    1115111111,1111115111,7777777577,9959999999,9995999999,9999959999, 
--    1711111111,1117111111,4444444447,5555555557,1111111181,3333333833,
--    8777777777,7778777777]
--------------------------------------------------------------------------------

specialPrimes = filter isPrime 
              . map (\p -> read p :: Integer) 
              . validPatterns



--------------------------------------------------------------------------------
-- At this stage we see that we have a solution for 9 digits d, with 
-- *   d = 1, 3, 4, 5, 6, 7, 9
--
-- So we are still missing 0 - as expected, 2 and 8. Our initial bet reveals
-- thus wrong. We have now to search for primes with only 8 same digits.
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Generate all combinaisons made of dots, then a 'a', then dots, then a 'b"
-- then dots. Any dot sections may be empty, but the totel length must be n.
--
-- Ex: getCombinaisons 4 = ["..21",".2.1","2..1",".21.","2.1.","21.."]
--------------------------------------------------------------------------------

genCombinaisons2 n = iter "ab" (take (n-2) $ repeat '.') [[]]
  where
    iter :: [Char] -> [Char] -> [[Char]] -> [[Char]]
    iter as@(a:as') es@(e:es') acc = acc'++acc''
      where
        acc'  = iter as' es $ map (a:) acc
        acc'' = iter as es' $ map (e:) acc

    iter as [] acc = map ((reverse as) ++) acc
    iter [] es acc = map (es ++) acc



--------------------------------------------------------------------------------
-- Generate all combinaisons of n digits (represented as chars) with (n-2)
-- identical digits equal to '0', 2' or '8' that are candidate as a prime
-- number.
--
-- Ex: patterns2 4 = ["2200","2020","0220","2002","0202", ... "9898","9988"]
--------------------------------------------------------------------------------

patterns2 n = concat $ map genNumbers'
                           [(a, b, dot) | a <- ['0'..'9']
                                        , b <- ['0'..'9']
                                        , dot <- "028"
                                        , a /= dot
                                        , b /= dot
                           ]
  where 
    patterns = genCombinaisons2 n
    genNumbers' (a, b, dot) = map (map reify) patterns
      where
        reify 'a' = a
        reify 'b' = b
        reify '.' = dot



--------------------------------------------------------------------------------
-- Generate all combinaisons of n digits (represented as chars) with (n-2)
-- identical digits equals to 2 and 8 that are candidate as a prime number.
--
-- Ex: validPatterns2 4 = ["2021","2201","8081", ..., "8989","8899"]
--------------------------------------------------------------------------------

validPatterns2 n = ps'''
  where
    -- ps are the candidate patterns in reverse order
    ps = patterns2 n

    -- The patterns not ending (i.e. starting seen the reverse order) by 1, 3, 7
    -- or 9 are discared:
    ps' = filter (\p -> elem (head p) "1379") ps

    -- Get the pattern in the right order
    ps'' = map reverse ps'

    -- The pattern must start with a non '0' digit
    ps''' = filter (\p -> head p /= '0') ps''

--------------------------------------------------------------------------------
-- Return the prime made of n digits with (n-2) identical digits being 2 or 8.
--
-- Ex: specialPrimes2 10 
-- 
-- [2022222221,8888808881,8888880881,2202222223,2222202223
-- ,2222220223,8880888883,2222220227,8888880887,8888888087,8888088889,8888888089
-- ,8888888809,1222222223,2222221223,1000000007,1000000009,2221222229,2222221229
-- ,2222222129,8888818889,8888881889,8888882881,8888888287,2232222221,8838888881
-- ,8888888383,2222322227,8888838887,8888888837,3222222229,2232222229,4222222223
-- ,2422222223,2224222223,2222222243,8488888883,8888488883,8888888483,4000000007
-- ,4000000009,2222242229,2222224229,8888848889,5222222221,2222225221,8885888881
-- ,6000000001,2222262221,2222222621,8888868881,6000000007,2222262227,2222222267
-- ,8868888887,2262222229,8688888889,8886888889,8888868889,7000000001,2272222223
-- ,7222222229,2722222229,2272222229,2222282221,9000000001,2292222221,2222229221
-- ,2222222921,2222222291,9888888881,8888898881,8888888891,2292222223,8898888883
-- ,2222222927,2222222297,9888888887,8888888989]

--------------------------------------------------------------------------------

specialPrimes2 = filter isPrime 
               . map (\p -> read p :: Integer) 
               . validPatterns2

--------------------------------------------------------------------------------
-- OK, this time we have all everything we need to solve the problem.
--------------------------------------------------------------------------------

main = print $ sum(specialPrimes 10) + sum(specialPrimes2 10) 