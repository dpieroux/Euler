{-------------------------------------------------------------------------------

We can easily verify that none of the entries in the first seven rows of
Pascal's triangle are divisible by 7:

                                 1
                               1   1
                             1   2   1
                           1   3   3   1
                         1   4   6   4   1
                       1   5  10  10   5   1
                     1   6  15  20  15   6   1

However, if we check the first one hundred rows, we will find that only 2361 of
the 5050 entries are not divisible by 7.

Find the number of entries which are not divisible by 7 in the first one billion
(10⁹) rows of Pascal's triangle.

--------------------------------------------------------------------------------

If one replaces the coefficients that are not divisible by 7 by a 'X' and those
divisible by 7 by a '.' when drawing the Pascal triangle, a regular shape
appears:

        X...........................
        XX..........................
        XXX.........................
        XXXX........................
        XXXXX.......................
        XXXXXX......................
        XXXXXXX.....................
        X......X....................
        XX.....XX...................
        XXX....XXX..................
        XXXX...XXXX.................
        XXXXX..XXXXX................
        XXXXXX.XXXXXX...............
        XXXXXXXXXXXXXX..............
        X......X......X.............
        XX.....XX.....XX............
        XXX....XXX....XXX...........
        XXXX...XXXX...XXXX..........
        XXXXX..XXXXX..XXXXX.........
        XXXXXX.XXXXXX.XXXXXX........
        XXXXXXXXXXXXXXXXXXXXX.......
        X......X......X......X......
        XX.....XX.....XX.....XX.....
        XXX....XXX....XXX....XXX....
        XXXX...XXXX...XXXX...XXXX...
        XXXXX..XXXXX..XXXXX..XXXXX..
        XXXXXX.XXXXXX.XXXXXX.XXXXXX.
        XXXXXXXXXXXXXXXXXXXXXXXXXXXX

The key point is that, if one scales by a factor 7, i.e. if every X represents
now 7 rows, i.e. if X is replaced by
        X......
        XX.....
        XXX....
        XXXX...
        XXXXX..
        XXXXXX.
        XXXXXXX,
the figure is left unchanged.

Let's call
    - a level-1 triangle a triangle whose height (resp. base) is made of 7 rows
      (resp. columns),
    - a level-2 triangle a triangle whose height and base are both made of 7
      level-1 triangles,
    - and more generally, a level-l triangle a triangle whose height and base
      are both made of 7 level-(l-1) triangles.

A level-1 triangle contains 1+2+...+7 = 28 coefficients not divisible by 7. For
a level-2 triangle, that number is 28^2, and for a level-l it becomes 28^l.

Let {n_e, ..., n_1, n_0} be the digits of n in base 7.

There are 1+2+...+n_e = n_e (n_e+1)/2 full triangles of level e contained in the
first n_e*7^e rows. The left {n_(e-1), ..., n_1, n_0} rows are divided in n_e+1
sections. Each section can recursively be decomposed into full triangles of
level e-1 and smaller.

This leads to a recursive solution.

-------------------------------------------------------------------------------}


-- Returns the digits in base 7 and the corresponding powers of a number
digits n = iter n 0 []
  where
    iter 0 _ acc = acc
    iter n e acc = let (q, r) = quotRem n 7 in iter q (e+1) ((r,e):acc)

sigma n = div (n*(n+1)) 2

euler n = iter (digits n)
  where
    iter ((d,e):ds) = (sigma d)*(28^e) + (d+1)*iter(ds)
    iter [] = 0

main = putStrLn
     $ concat ["Euler 148, 10^2 rows:", show $ euler (10^2), "\n"
              ,"           10^9 rows:", show $ euler (10^9), "\n"]