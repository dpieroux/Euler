{-------------------------------------------------------------------------------

Rectangles in cross-hatched grids Problem 147

In a 3x2 cross-hatched grid, a total of 37 different rectangles could be
situated within that grid as indicated in the sketch.

There are 5 grids smaller than 3x2, vertical and horizontal dimensions being
important, i.e. 1x1, 2x1, 3x1, 1x2 and 2x2. If each of them is cross-hatched,
the following number of different rectangles could be situated within those
smaller grids:

1x1: 1 2x1: 4 3x1: 8 1x2: 4 2x2: 18

Adding those to the 37 of the 3x2 grid, a total of 72 different rectangles could
be situated within 3x2 and smaller grids.

How many different rectangles could be situated within 47x43 and smaller grids?

--------------------------------------------------------------------------------

So, more generally we have to compute how many different rectangles could be situated within a rxc (Rows x Columns) and smaller grids?

We can apply a multi-axe divide and conquer approach to solve this problem.

Notation
--------

total(r,c): number of rectangles situated within a main rxc grid.

straight(r,c): number of those rectangles in a rxc grid which are straight, that
is which are aligned with the main grid.

dStraight(r,c): number of straight rectangles in a rxc grid which overlap with
the first column

slanted(r,c): number of those rectangles in a rxc grid which are slanted of 45°
relative to the main grid.

dSlanted(r,c): number of slanted rectangles in a rxc grid which overlap with the
first column


Basic properties
----------------

The first three properties enable a multi-axis divide-and-conquer approach.

 1. The problem is composed in two independent sub-problems: one for straight
    rectangles, one for slant rectangles:

        total(r, c) = straight(r, c) + slanted(r, c)


 2. The number of straight rectangles in a rxc grid is equal to the number of
    rectangles that overlap with the first column plus the number of rectangles
    which don't:

        straight(r, c) = straight(r, c-1) + dStraight(r,c)
        slanted(r, c)  = slanted(r, c-1)  + dSlanted(r,c)

        total(r, c) = total(r, c) + dStraight(r,c) + dSlanted(r,c)


 3. The fourth one is the 'base cases':

        straight(0, c) = 0
        slanted(0, c)  = 0


 4. There are r(r+1)/2 straight rectangles limited to the first column. Each of
    them can be extended to the other columns. Thus, there are r*(r+1)*c/2
    rectangles that overlap the first column:

        dStraight(r,c) = r*(r+1)*c/2

 5. The next task is to count the slanted rectangles that overlap with the first
    column. Let (x0, y0) be the coordinates of A, the left most corner, A. That
    corner can either be located on the grid border (xA=1), or at a distance of
    1/2 from it (xA=1/2). The acceptable values of y are then:

        1) xA = 0  ; yA =   1,   2, ..., r-1;
        2) xA = 1/2; yA = 1/2, 3/2, ..., r-1/2 = (1, 3, ..., 2r-1) / 2

    Let u (resp. v) be the length of the side going up (resp. down)

                B
                /\
               /  \ v
            u /    \ C
             /     /
          A /     /
            \    / u
           v \  /
              \/
               D

    B, C and D must be located within the grid. This implies:

            yB = yA + 1/2 + (u-1)/2 ≤ r            =>     1 ≤ u ≤ 2 (r-yA)
        0 ≤ yD = yA - 1/2 - (v-1)/2                =>     1 ≤ v ≤ 2 yA
            xC = xA +   1 + (u-1)/2 + (v-1)/2 ≤ c  =>   u+v ≤ 2 (c-xA)

    We can rewrite the first two equations by accounting for the third one:

        1 ≤ u ≤ 2 min (2(r-yA), 2(c-xA)-1;
        1 ≤ v ≤ min (2 yA, 2 (c-xA) - u)

    To avoid having to deal with half integral xA and yA, let introduce xxA =
    2xA and yyA = 2ya:

        1 ≤ u ≤ min (2r - yyA, 2c - xxA - 1);
        1 ≤ v ≤ min (yyA, 2c-xxA - u)

    The number of slanted rectangles starting at A is the number of different
    pairs (u, v) verifying these constraints.

-------------------------------------------------------------------------------}

import Data.Array

dStraight r c = quot (r*(r+1)*c) 2

dSlanted  r c = count 0 [2, 4 .. 2*(r-1)] + count 1 [1, 3 .. 2*r-1]
  where
    count xxA yyAs = sum [ min yyA (2*c - xxA - u)
                         | yyA <- yyAs
                         , u  <- [1 .. min (2*r-yyA) (2*c-xxA-1)]
                         ]

euler :: Int -> Int -> Int
euler r c = sum $ elems total
  where
    total = array ((0, 0), (r, c))
                  (  [ ((i, 0), 0) | i <- [0 .. r]]
                  ++ [ ((0, j), 0) | j <- [1 .. c]]
                  ++ [ ((i, j), total!(i, j-1) + dStraight i j + dSlanted i j)
                        | i <- [1 .. r], j <- [1 .. c]]
                  )

main = do
    putStrLn $ concat ["Euler 137, 3x2:   ", show $ euler 3 2]
    putStrLn $ concat ["Euler 137, 47x43: ", show $ euler 47 43]